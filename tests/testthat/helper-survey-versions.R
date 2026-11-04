# Hand-made API responses for the survey-versions tests. These mimic the shape
# of the Qualtrics API responses; they contain no real survey content.

# Versions list, in the shape of `result$elements` from
# GET survey-definitions/{surveyId}/versions:
mock_versions <- function() {
  list(
    list(metadata = list(
      surveyID = "SV_abcdef123456789",
      versionID = "9000000000000003",
      versionNumber = 3L,
      description = "Draft changes",
      userID = "UR_abcdef123456789",
      creationDate = "2024-03-01T12:00:00Z",
      published = FALSE,
      wasPublished = FALSE,
      publishEvents = NULL
    )),
    list(metadata = list(
      surveyID = "SV_abcdef123456789",
      versionID = "9000000000000002",
      versionNumber = 2L,
      description = "Second launch",
      userID = "UR_abcdef123456789",
      creationDate = "2024-02-01T12:00:00Z",
      published = TRUE,
      wasPublished = TRUE,
      publishEvents = list(list(date = "2024-02-01T12:00:00Z",
                                userID = "UR_abcdef123456789"))
    )),
    list(metadata = list(
      surveyID = "SV_abcdef123456789",
      versionID = "9000000000000001",
      versionNumber = 1L,
      description = "First launch",
      userID = "UR_abcdef123456789",
      creationDate = "2024-01-01T12:00:00Z",
      published = FALSE,
      wasPublished = TRUE,
      publishEvents = list(list(date = "2024-01-01T12:00:00Z",
                                userID = "UR_abcdef123456789"))
    ))
  )
}

# Survey definition, in the shape returned by both
# GET survey-definitions/{surveyId} and .../versions/{versionId}:
mock_definition <- function(name = "Mock survey") {
  list(result = list(
    SurveyName = name,
    SurveyStatus = "Active",
    SurveyID = "SV_abcdef123456789",
    LastModified = "2024-03-01T12:00:00Z",
    ProjectInfo = list(SurveyID = "SV_abcdef123456789",
                       ProjectCategory = "CORE"),
    SurveyOptions = list(BackButton = "false"),
    SurveyFlow = list(Type = "Root", FlowID = "FL_1"),
    Blocks = list(BL_1 = list(Type = "Default")),
    Questions = list(QID1 = list(QuestionText = "Q1")),
    ResponseSets = list(RS_1 = list()),
    Scoring = list()
  ))
}
