#' Fetch a list from toggl using the v8 api.
#'
#' @param toggl_token A toggl API token to access https://toggl.com.
#' @param workspace_id The workspace id for the wanted workspace.
#' @param what What to fetch
#' @param verbose A flag to enable more verbose output, Default value: FALSE
#' @return The list (JSON) of clients accessable using \code{toggl_token} from the toggl workspace with id: \code{workspace_id}.
#' @family get.toggl
#' @examples
#' get.toggl.v8(Sys.getenv("TOGGL_TOKEN"), Sys.getenv("TOGGL_WORKSPACE"), "clients")
#' @export
get.toggl.v8 <- function(toggl_token, workspace_id, what, verbose = FALSE) {
  username <- toggl_token
  password <- "api_token"

  base <- "https://toggl.com/api"
  endpoint <- "v8/workspaces"

  call <- paste(base, endpoint, workspace_id, what, sep = "/")

  if (verbose) {
    result <- GET(call, authenticate(username, password), verbose())
  } else {
    result <- GET(call, authenticate(username, password))
  }
  return(result)
}
#' Fetch a list from toggl using the v8 api.
#'
#' @param toggl_token A toggl API token to access https://toggl.com.
#' @param workspace_id The workspace id for the wanted workspace.
#' @param what What to fetch
#' @param verbose A flag to enable more verbose output, Default value: FALSE
#' @return The list (JSON) of clients accessable using \code{toggl_token} from the toggl workspace with id: \code{workspace_id}.
#' @family get.toggl
#' @examples
#' get.toggl.v8(Sys.getenv("TOGGL_TOKEN"), Sys.getenv("TOGGL_WORKSPACE"), "clients")
#' @export
get.toggl.v8.data <- function(toggl_token, workspace_id, what, verbose = FALSE) {
  json.response <- get.toggl.v8(toggl_token, workspace_id, what, verbose)

  if (json.response$status_code == 200) {
    return(fromJSON(content(json.response, "text")))
  }

  print(json.response)
  return("ERROR")
}
#' Fetch a list of clients from toggl.
#'
#' @param toggl_token A toggl API token to access https://toggl.com.
#' @param workspace_id The workspace id for the wanted workspace.
#' @param verbose A flag to enable more verbose output, Default value: FALSE
#' @return The list (JSON) of clients accessable using \code{toggl_token} from the toggl workspace with id: \code{workspace_id}.
#' @family get.toggl
#' @examples
#' get.toggl.clients(Sys.getenv("TOGGL_TOKEN"), Sys.getenv("TOGGL_WORKSPACE"))
#' @export
get.toggl.clients <- function(toggl_token, workspace_id, verbose = FALSE) {
  return(get.toggl.v8.data(toggl_token, workspace_id, "clients"))
}

#' Fetch a list of groups from toggl.
#'
#' @param toggl_token A toggl API token to access https://toggl.com.
#' @param workspace_id The workspace id for the wanted workspace.
#' @param verbose A flag to enable more verbose output, Default value: FALSE
#' @return The list (JSON) of groups accessable using \code{toggl_token} from the toggl workspace with id: \code{workspace_id}.
#' @family get.toggl
#' @examples
#' get.toggl.groups(Sys.getenv("TOGGL_TOKEN"), Sys.getenv("TOGGL_WORKSPACE"))
#' @export
get.toggl.groups <- function(toggl_token, workspace_id, verbose = FALSE) {
  return(get.toggl.v8.data(toggl_token, workspace_id, "groups"))
}


#' @export
get.group.details <- function(toggl_token, workspace_id, group, since = Sys.Date() - 7, until = Sys.Date(), page = 1, verbose = FALSE) {
  username <- toggl_token
  password <- "api_token"

  base <- "https://toggl.com/reports/api"
  endpoint <- "v2/details?"

  what <- paste("workspace_id=", workspace_id, sep = "")
  what <- paste(what, "&since=", since, "&until=", until, sep = "")
  what <- paste(what, "&user_agent=api_test", sep = "")
  what <- paste(what, "&members_of_group_ids=", group, sep = "")
  what <- paste(what, "&billable=both", sep = "")
  what <- paste(what, "&page=", page, sep = "")

  call <- paste(base, endpoint, sep = "/")
  call <- paste(call, what, sep = "")

  print(call)

  if (verbose) {
    result <- GET(call, authenticate(username, password), verbose())
  } else {
    result <- GET(call, authenticate(username, password))
  }
  return(result)
}

#' @export
get.group.data <- function(toggl_token, workspace_id, group, verbose = FALSE) {
  page <- 1

  not.done <- TRUE

  while (not.done) {
    json.response <- get.group.details(toggl_token, workspace_id, group, page = page)
    print(json.response)
    if (json.response$status_code == 200) {
      response <- fromJSON(content(json.response, "text", encoding = 'UTF-8'))
      print(summary(response))
      print(rownames(response$data))
      print(response$total_count)
      print(length(response$data))
      if (length(response$data) > 0) {
        if (page == 1) {
          data.response <- data.frame(response$data)
        } else {
          data.response <- rbind(data.response, response$data)
        }
      } else {
        not.done <- FALSE
      }
    } else {
      not.done <- FALSE
    }
    page <- page + 1
    Sys.sleep(1)
  }
  return(data.response)
}
