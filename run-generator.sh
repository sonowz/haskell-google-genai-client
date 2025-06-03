openapi-generator-cli generate \
  --input-spec genai_openapi.json \
  --generator-name haskell-http-client \
  -p baseModule=GenAI.Client \
  -p cabalPackage=haskell-google-genai-client \
  -p configType=GenAIClientConfig \
  -p useKatip=false