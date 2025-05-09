#' @export
#'
#' @title Channel nodes to spatial stream network
#'
#' @description
#'
#' @param nodes
#' @param lsn_path
#' @param ssn_path
#' @param overwrite
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table setkey
#'
#'
#' @return Invisibly return the .ssn object

nodes_to_ssn <- function(nodes,
                         lsn_path,
                         ssn_path,
                         obs,
                         preds = "auto",
                         weights_param = "AREA_SQKM",
                         parallel = FALSE,
                         ncores = parallel::detectCores(),
                         check_topology = FALSE,
                         snap_tolerance = .1,
                         topo_tolerance = .5,
                         save_local = TRUE,
                         overwrite = FALSE) {



  # TEST
  nodes <- sf::read_sf("inst/examples/nodes_of_interest.shp")
  lsn_path = tempdir()
  ssn_path = file.path(lsn_path, "ssn.ssn")
  # Create adjacency matrix
  # first need to index nodes from 1:n

  ## Checks
  # crs

  .make_A <- function(n_dt, N) {
    e <- n_dt[, .(from = nid, to = to_nid)]
    e <- e[!is.na(to)]

    Matrix::sparseMatrix(i = e$from,
                 j = e$to,
                 x = 1,
                 dims = c(N, N))
  }
  n <- data.table::as.data.table(nodes)
  data.table::setkey(n, NodeNum)
  n$nid <- 1:nrow(n)

  # connect to downstream node using new ids
  # This uses the data.table key to find the row where NodeNum == ToNode, then get
  # the new nid, and assign that to a new to_nid column
  n$to_nid <- n[.(ToNode), nid]
  data.table::setkey(n, nid)


  # Now programmatically fix the other errors
  N <- nrow(n)
  A <- .make_A(n, N)

  # SSN does not allow confluences of more than two streams. Where this happens,
  # make one of them (the one with the smallest contributing area to avoid editing
  # the main channel) converge one node farther down the network
  complex_confluences <- which(Matrix::colSums(A) == 3)

  for (c in complex_confluences) {
    from_n <- n[to_nid == c]
    # to ensure that we move one of the side nodes and not the middle one,
    # check the pairwise distances. The pair which are farthest from each other
    # should be the two side channels.
    from_n_sf <- nodes[nodes$NodeNum %in% from_n$NodeNum, ]
    dists <- st_distance(from_n_sf)
    max_dist <- which.max(dists)  # 3x3 mx
    sides <- c(max_dist %/% 3, ifelse(max_dist %% 3 == 0, 3, max_dist %% 3))
    from_n_sides <- from_n[NodeNum %in% from_n_sf[sides, ]$NodeNum]
    # choose the side with smallest contributing area
    n[.(from_n_sides[which.min(from_n_sides$AREA_SQKM), nid]),  to_nid := n[.(c), to_nid]]
  }


  # start with only sources, outlets, and confluences
  sources <- which(Matrix::colSums(A) == 0)
  outlets <- which(Matrix::rowSums(A) == 0)
  confluences <- which(Matrix::colSums(A) == 2)

  # now make sure that there are no outlets which are also confluences.
  converging <- outlets[outlets %in% confluences]



  # Move the smaller one up one for each of these
  # This fails on 8916 because the lines still intersect.
  # for just this one, also delete the previous node.
  # TODO: figure out a better programmatic way to do this.

  # if (c == 8916) {
  #   n <- n[nid != 8915]
  #   n[to_nid == 8915, to_nid := 8917]
  # }

  for (c in converging) {
    from_n <- n[to_nid == c]
    n[.(from_n[which.min(from_n[[weights_param]]), nid]),
      to_nid := from_n[which.max(from_n[[weights_param]]), nid]]
  }

  A <- .make_A(n, N)

  # create new data tables which will hold info for reduced network

  # start with only sources, outlets, and confluences
  sources <- which(Matrix::colSums(A) == 0)
  outlets <- which(Matrix::rowSums(A) == 0)
  confluences <- which(Matrix::colSums(A) == 2)

  # exclude sources with 0 contributing area
  sources <- n[nid %in% sources & n[[weights_param]] > 0]$nid

  # Start with node ids for only sources and confluences. Each of these
  # will correspond to one edge.

  new_n <- data.table::data.table(old_id = c(sources, confluences))
  data.table::setorder(new_n, old_id)
  new_n$new_id <- 1:nrow(new_n)
  new_e <- data.table::data.table(from = new_n$new_id)
  new_n <- rbind(new_n, data.table::data.table(old_id = outlets, new_id = 1:length(outlets) + nrow(new_n)))


  # For each source or confluence node, get all nodes between it and the next
  # downstream confluence or outlet

  # recursive function to get all nodes (in order) from one confluence to the
  # next downstream one.
  get_downstream <- function(id, i = 1, n_list = id) {
    to = n[id, to_nid]
    # Don't include node 8915 because it makes a converging node (outlet and confluence)
    # TODO: figure out a better programmatic way to do this.
    if (to != 8915) {
      n_list <- c(n_list, to)
    }
    i = i+1
    # id <- to
    # to %in% c(confluences, outlets)
    if (to %in% c(confluences, outlets)) {
      return(list(to = new_n[old_id == to, new_id], i = i, n_list = n_list))
    } else {
      get_downstream(to, i, n_list)
    }
  }

  # This takes a while for all 28k or so points so let's do it in parallel.
  if (parallel) {
    n_cores <- parallel::detectCores()
    cl <- parallel::makeCluster(n_cores)
    clusterExport(cl, c("n", "confluences", "outlets", "new_n", "get_downstream"))
    sets <- parLapply(cl, new_n[new_e$from, old_id], get_downstream)
  } else {
    sets <- lapply(new_n[new_e$from, old_id], get_downstream)
  }

  # Now use the list of nodes associated with each edge to get geometry for the
  # edge.
  n_sf <- sf::st_as_sf(n)
  if (parallel) {
    clusterExport(cl, "n_sf")
    geometryList <- parLapply(cl, sets, \(s) {sf::st_linestring(sf::st_coordinates(n_sf)[s$n_list, ])})
  } else {
    geometryList <- lapply(sets, \(s) {sf::st_linestring(sf::st_coordinates(n_sf)[s$n_list, ])})
  }


  new_e$geometry <- sf::st_as_sfc(geometryList)
  # record contributing area at most downstream point to use for SSN weighting

  if (parallel) {
    weights <- parLapply(cl, sets, \(s) {
      n[nid == s$n_list[length(s$n_list) - 1]][[weights_param]]
    })
    parallel::stopCluster(cl)
  } else {
    weights <- lapply(sets, \(s) {
      n[nid == s$n_list[length(s$n_list)]][[weights_param]]
    })
  }

  new_e$weight <- unlist(weights)


  new_e$to <- sapply(sets, \(s) s$to)
  new_e$rid <- 1:nrow(new_e)
  new_e <- sf::st_as_sf(new_e, crs = sf::st_crs(nodes))

  # get nids so we can get info from nodes table later

  data.table::setkey(new_n, new_id)
  new_e$from_nodenum <- n[.(new_n[.(new_e$from), old_id]), NodeNum]
  new_e$to_nodenum <- n[.(new_n[.(new_e$to), old_id]), NodeNum]

  ## PREDICTIONS AND OBSERVATIONS
  if (preds == "auto") {
    # get nids of center node of edge for predictions
    new_e$center_nodenum <- n[.(unlist(lapply(sets, \(s) s$n_list[round(s$i/2)]))), NodeNum]
    data.table::setkey(n, NodeNum)
    new_e$node_count <- unlist(lapply(sets, \(s) s$i))
    pred_points <- sf::st_as_sf(n[.(new_e[new_e$node_count > 2, ]$center_nodenum)])
  }

  new_e$rid <- NULL
  edges <- SSNbler::lines_to_lsn(
    streams = sf::st_as_sf(as.data.frame(new_e), crs = sf::st_crs(new_e)),
    lsn_path = lsn_path,
    check_topology = check_topology,
    snap_tolerance = snap_tolerance,
    topo_tolerance = topo_tolerance,
    overwrite = overwrite
  )
  obs <- SSNbler::sites_to_lsn(
    sites = obs,
    edges = edges,
    lsn_path = lsn_path,
    file_name = "obs",
    snap_tolerance = snap_tolerance,
    save_local = save_local,
    overwrite = overwrite
  )


}

