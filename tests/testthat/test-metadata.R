library(webmockr)

context("Get metadata for a survey.")

metadata_factory <- list(
  response = list(
    OK = TRUE,
    content = list(
      result = list(
        id = "SV_123456789012345",
        name = "Example Survey",
        ownerId = "UR_abcdefghijklmno",
        organizationId = "Example Organization",
        isActive = FALSE,
        creationDate = "2017-05-09T16:07:12Z",
        lastModifiedDate = "2017-05-09T16:10:31Z",
        expiration = list(
          startDate = NULL,
          endDate = NULL
        ),
        questions = list(
          QID1 = list(
            questionType = list(
              type = "TE",
              selector= "SL",
              subSelector = NULL
            ),
            questionText = "Text Entry Question",
            questionLabel = NULL,
            validation = list(
              doesForceResponse = FALSE
            ),
            questionName = "Q1"
          ),
          QID2 = list(
            questionType = list(
              type = "TE",
              selector= "SL",
              subSelector = NULL
            ),
            questionText = "Text Entry Question",
            questionLabel = NULL,
            validation = list(
              doesForceResponse = FALSE
            ),
            questionName = "Q2"
          ),
          QID3 = list(
            questionType = list(
              type = "Matrix",
              selector= "Likert",
              subSelector = "SingleAnswer"
            ),
            questionText = "Matrix Question",
            questionLabel = NULL,
            validation = list(
              doesForceResponse = FALSE
            ),
            questionName = "Q3",
            subQuestions = list(
              `1` = list(
                recode = "1",
                description = "Statement 1",
                choiceText = "Statement 1",
                imageDescription = NULL,
                variableName = NULL
              ),
              `2` = list(
                recode = "2",
                description = "Statement 2",
                choiceText = "Statement 2",
                imageDescription = NULL,
                variableName = NULL
              )
            ),
            choices = list(
              `1` = list(
                recode = "1",
                description = "Scale Point 1",
                choiceText = "Scale Point 1",
                imageDescription = NULL,
                variableName = NULL,
                analyze = TRUE
              ),
              `2` = list(
                recode = "2",
                description = "Scale Point 2",
                choiceText = "Scale Point 2",
                imageDescription = NULL,
                variableName = NULL,
                analyze = TRUE
              )
            )
          )
        ),
        blocks = list(
          "BL_9ozqTRwJd1omzYh" = list(
            description = "Default Question Block",
            elements = c(
              list(
                type = "Question",
                questionId = "QID1"
              ),
              list(
                type = "Question",
                questionId = "QID2"
              )
            )
          )
        ),
        flow = c(
          list(
            id = "BL_9ozqTRwJd1omzYh",
            type = "Block"
          )
        ),
        embeddedData = c(
          list(
            name = "Embedded Data Field",
            value = "Embedded Data Field value"
          )
        ),
        responseCounts = list(
          auditable = 0,
          generated = 0,
          deleted = 0
        ),
        exportColumnMap = list(
          Q1 = list(
            question = "QID1"
          ),
          Q2 = list(
            question = "QID2"
          ),
          `Q3_1(1)` = list(
            question = "QID3",
            subQuestion = "QID3.subQuestions.1"
          ),
          `Q3_2(1)` = list(
            question = "QID3",
            subQuestion = "QID3.subQuestions.2"
          ),
          `Q3_1(2)` = list(
            question = "QID3",
            subQuestion = "QID3.subQuestions.1"
          ),
          `Q3_2(2)` = list(
            question = "QID3",
            subQuestion = "QID3.subQuestions.2"
          )
        ),
        meta = list(
          httpStatus = "200 - OK",
          requestId = "7309eb77-db6a-47bb-b818-4341aaca58f7"
        )
      )
    )
  )
)

test_that("metadata() should throw an error if passing invalid options to input", { # nolint
  local_mock(`qualtRics::qualtrics_api_request` =
               function(verb, url) metadata_factory$response$content)
  qualtrics_api_credentials("api_key", "test.qualtrics.com")

  expect_error(
    metadata("mockId1", get = list(invalidKey=TRUE))
  )

})

test_that("metadata() should throw warning if input questions are not a vector", { # nolint
  local_mock(`qualtRics::qualtrics_api_request` =
               function(verb, url) metadata_factory$response$content)
  qualtrics_api_credentials("api_key", "test.qualtrics.com")

  expect_warning(
    metadata("mockId1", questions = "I am not a vector")
  )
})

test_that("metadata() should make the proper request to Qualtrics", {
  webmockr::enable()
  # We mock dependent functions since we are only testing the requests
  local_mock(`qualtRics::check_for_warnings` = function(resp) NULL)
  local_mock(`qualtRics::qualtrics_response_codes` =
               function(verb, url) metadata_factory$response)

  # Define expectations
  expected <- list(
    verb     = "GET",
    host     = "https://test.qualtrics.com",
    path     = "/API/v3/surveys/",
    id       = "sv1",
    api_key  = "1234",
    headers  = list(
      'X-API-TOKEN' = "1234",
      'Content-Type' = 'application/json',
      'Accept' = '*/*',
      'accept-encoding' = 'gzip, deflate'
    )
  )

  qualtrics_api_credentials(expected$api_key, expected$host)
  # Stub our request to check if expected request was made
  stub_request(
    expected$verb, paste0(expected$host, expected$path, expected$id)
  ) %>%
    wi_th(headers = expected$headers) %>%
    to_return(status=200)

  # Will throw error if expected request not made
  expect_is(metadata(expected$id), "list")

  stub_registry_clear()
  webmockr::disable()
})

test_that("metadata() should throw warning and return all questions if input question does not exist in metadata", { # nolint
  local_mock(`qualtRics::qualtrics_api_request` =
               function(verb, url) metadata_factory$response$content)
  qualtrics_api_credentials("api_key", "test.qualtrics.com")

  res <- suppressWarnings(metadata("mockId1"))
  expected_questions <- c("QID1", "QID2", "QID3")
  all_questions_outputted <- all(names(res$questions) %in% expected_questions) &
    all(expected_questions %in% names(res$questions))
  expect_true(all_questions_outputted)
})

test_that("metadata() should add non-standard options if specified", {
  local_mock(`qualtRics::qualtrics_api_request` =
               function(verb, url) metadata_factory$response$content)
  qualtrics_api_credentials("api_key", "test.qualtrics.com")

  res <- metadata("mockId1", get = list(
    flow=TRUE
  ))

  expected_output_names <- c("flow", "responsecounts", "questions", "metadata")
  output_matches <- all(names(res) %in% expected_output_names) &
    all(expected_output_names %in% names(res))
  expect_true(output_matches)
})
