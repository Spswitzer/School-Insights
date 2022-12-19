observe({ 
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query[['SchoolID']])) {
    updateTextInput(session, "SchoolID", value = query[['SchoolID']])
  }
})