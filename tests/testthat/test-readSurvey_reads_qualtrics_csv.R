test_that("readSurvey() reads data in qualtrics format and converts columns", {
  # Set working directory
  setwd(tempdir())
  # Make mock data that looks like qualtrics csv export
  test.qualtrics.data <- list(
    "ResponseID"=c("ResponseID","{'ImportId': 'responseId'}", "test1"),
    "ResponseSet"=c("ResponseSet","{'ImportId': 'responseSetId'}","Default Response Set"),
    "IPAddress"=c("IPAddress", "{'ImportId': 'ipAddress'}", "11.123.123.123"),
    "StartDate"=c("StartDate", "{'ImportId': 'startDate'}", "2016-04-04 10:20:41"),
    "EndDate"=c("EndDate","{'ImportId': 'endDate'}","2016-04-04 10:22:13"),
    "RecipientLastName"=c("RecipientLastName","{'ImportId': 'panel-RecipientLastName'}",""),
    "RecipientFirstName"=c("RecipientFirstName","{'ImportId': 'panel-RecipientFirstName'}",""),
    "RecipientEmail"=c("RecipientEmail","{'ImportId': 'panel-RecipientEmail'}", ""),
    "ExternalDataReference"=c("ExternalDataReference","{'ImportId': 'panel-ExternalDataReference'}",""),
    "Finished"=c("Finished","{'ImportId': 'finished'}","1"),
    "Status"=c("Status","{'ImportId': 'status'}","0"),
    "Q1"=c("I am question 1", "{'ImportId': 'QID1'}", "answer"),
    "LocationLongitude"=c("LocationLatitude","{'ImportId': 'Location-LocationLatitude'}","98.254"),
    "LocationLatitude"=c("LocationLongitude","{'ImportId': 'Location-LocationLongitude'}","-111.11"),
    "LocationAccuracy"=c("LocationAccuracy","{'ImportId': 'Location-LocationAccuracy'}","-1")
  )
  # Write to temp file
  io <- data.frame(test.qualtrics.data)
  write.csv(io, "io.csv", row.names = FALSE)
  # Test if can read
  survey <- suppressWarnings(qualtRics::readSurvey("io.csv",convertStandardColumns = TRUE))
  # Tests
  expect_equal(dim(survey)[1], 1)
  expect_equal(dim(survey)[2], 15)
  expect_true(is.numeric(as.numeric(survey$StartDate)))
  expect_true(is.numeric(survey$LocationLatitude))
  expect_true(is.factor(survey$ResponseSet))
})
