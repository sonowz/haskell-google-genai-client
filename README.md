# Haskell Google GenAI Client

Unofficial low-level Haskell client for the Google Gemini API (also known as GenAI API or Generative Language API).

This library provides a type-safe way to interact with Google's Gemini API services directly from your Haskell code.

## Note
- The code is **automatically generated** from the OpenAPI specification using [openapi-generator](https://openapi-generator.tech/).
- APIs include text/image/audio generation, embeddings, model tuning, semantic retrieval API, and more. See [Google API reference](https://ai.google.dev/api) for full details.


# Development

First, run `nix develop` to enter the development shell. Non-Nix users should install the dependencies such as openapi-generator and pre-commit.

```bash
# Generate the Haskell client code from the OpenAPI specification
./run-generator.sh
# Format the code and check for linting issues
pre-commit run --all-files
# Build the project
cabal build
# Run the tests
cabal test
```

# Example Usage

Here is a working example that uses 'generateContent' API to generate text using Gemini model:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import GenAI.Client
import Network.HTTP.Client (responseBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (QueryItem)
import Data.ByteString.Lazy qualified as LBS

apiKeyParam :: QueryItem
apiKeyParam = ("key", Just "<GEMINI_API_KEY>")

model :: Model2
model = Model2 "gemini-2.0-flash-001"

requestBody :: GenerateContentRequest
requestBody = mkGenerateContentRequest [content] "gemini-2.0-flash-001" where
  content = mkContent { contentParts = Just [textPart] }
  textPart = mkPart { partText = Just query }
  query = "What is the capital of Korea?"

request :: ClientRequest GenerateContent MimeJSON GenerateContentResponse MimeJSON
request = flip addQuery [apiKeyParam] $ flip setBodyParam requestBody $ generateContent model

main :: IO ()
main = do
  config <- withStdoutLogging =<< newConfig
  manager <- newTlsManager
  response <- dispatchLbs manager config request
  LBS.putStr $ responseBody response
```

Running code above will produce:
```json
{
  "candidates": [
    {
      "content": {
        "parts": [
          {
            "text": "The capital of South Korea is **Seoul**.\n"
          }
        ],
        "role": "model"
      },
      "finishReason": "STOP",
      "avgLogprobs": -0.024313041940331459
    }
  ],
  "usageMetadata": {
    "promptTokenCount": 7,
    "candidatesTokenCount": 8,
    "totalTokenCount": 15,
    "promptTokensDetails": [
      {
        "modality": "TEXT",
        "tokenCount": 7
      }
    ],
    "candidatesTokensDetails": [
      {
        "modality": "TEXT",
        "tokenCount": 8
      }
    ]
  },
  "modelVersion": "gemini-2.0-flash-001",
  "responseId": "ZQc_aPa7JcXWnvgP3LDNsAw"
}
```