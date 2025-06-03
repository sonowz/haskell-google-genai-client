{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import GenAI.Client.Model
import GenAI.Client.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary AttributionSourceId where
  arbitrary = sized genAttributionSourceId

genAttributionSourceId :: Int -> Gen AttributionSourceId
genAttributionSourceId n =
  AttributionSourceId
    <$> arbitraryReducedMaybe n -- attributionSourceIdGroundingPassage :: Maybe GroundingPassageId
    <*> arbitraryReducedMaybe n -- attributionSourceIdSemanticRetrieverChunk :: Maybe SemanticRetrieverChunk
  
instance Arbitrary BaseOperation where
  arbitrary = sized genBaseOperation

genBaseOperation :: Int -> Gen BaseOperation
genBaseOperation n =
  BaseOperation
    <$> arbitraryReducedMaybe n -- baseOperationDone :: Maybe Bool
    <*> arbitraryReducedMaybe n -- baseOperationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- baseOperationError :: Maybe Status
  
instance Arbitrary BatchCreateChunksRequest where
  arbitrary = sized genBatchCreateChunksRequest

genBatchCreateChunksRequest :: Int -> Gen BatchCreateChunksRequest
genBatchCreateChunksRequest n =
  BatchCreateChunksRequest
    <$> arbitraryReduced n -- batchCreateChunksRequestRequests :: [CreateChunkRequest]
  
instance Arbitrary BatchCreateChunksResponse where
  arbitrary = sized genBatchCreateChunksResponse

genBatchCreateChunksResponse :: Int -> Gen BatchCreateChunksResponse
genBatchCreateChunksResponse n =
  BatchCreateChunksResponse
    <$> arbitraryReducedMaybe n -- batchCreateChunksResponseChunks :: Maybe [Chunk]
  
instance Arbitrary BatchDeleteChunksRequest where
  arbitrary = sized genBatchDeleteChunksRequest

genBatchDeleteChunksRequest :: Int -> Gen BatchDeleteChunksRequest
genBatchDeleteChunksRequest n =
  BatchDeleteChunksRequest
    <$> arbitraryReduced n -- batchDeleteChunksRequestRequests :: [DeleteChunkRequest]
  
instance Arbitrary BatchEmbedContentsRequest where
  arbitrary = sized genBatchEmbedContentsRequest

genBatchEmbedContentsRequest :: Int -> Gen BatchEmbedContentsRequest
genBatchEmbedContentsRequest n =
  BatchEmbedContentsRequest
    <$> arbitraryReduced n -- batchEmbedContentsRequestRequests :: [EmbedContentRequest]
  
instance Arbitrary BatchEmbedContentsResponse where
  arbitrary = sized genBatchEmbedContentsResponse

genBatchEmbedContentsResponse :: Int -> Gen BatchEmbedContentsResponse
genBatchEmbedContentsResponse n =
  BatchEmbedContentsResponse
    <$> arbitraryReducedMaybe n -- batchEmbedContentsResponseEmbeddings :: Maybe [ContentEmbedding]
  
instance Arbitrary BatchEmbedTextRequest where
  arbitrary = sized genBatchEmbedTextRequest

genBatchEmbedTextRequest :: Int -> Gen BatchEmbedTextRequest
genBatchEmbedTextRequest n =
  BatchEmbedTextRequest
    <$> arbitraryReducedMaybe n -- batchEmbedTextRequestRequests :: Maybe [EmbedTextRequest]
    <*> arbitraryReducedMaybe n -- batchEmbedTextRequestTexts :: Maybe [Text]
  
instance Arbitrary BatchEmbedTextResponse where
  arbitrary = sized genBatchEmbedTextResponse

genBatchEmbedTextResponse :: Int -> Gen BatchEmbedTextResponse
genBatchEmbedTextResponse n =
  BatchEmbedTextResponse
    <$> arbitraryReducedMaybe n -- batchEmbedTextResponseEmbeddings :: Maybe [Embedding]
  
instance Arbitrary BatchUpdateChunksRequest where
  arbitrary = sized genBatchUpdateChunksRequest

genBatchUpdateChunksRequest :: Int -> Gen BatchUpdateChunksRequest
genBatchUpdateChunksRequest n =
  BatchUpdateChunksRequest
    <$> arbitraryReduced n -- batchUpdateChunksRequestRequests :: [UpdateChunkRequest]
  
instance Arbitrary BatchUpdateChunksResponse where
  arbitrary = sized genBatchUpdateChunksResponse

genBatchUpdateChunksResponse :: Int -> Gen BatchUpdateChunksResponse
genBatchUpdateChunksResponse n =
  BatchUpdateChunksResponse
    <$> arbitraryReducedMaybe n -- batchUpdateChunksResponseChunks :: Maybe [Chunk]
  
instance Arbitrary Blob where
  arbitrary = sized genBlob

genBlob :: Int -> Gen Blob
genBlob n =
  Blob
    <$> arbitraryReducedMaybe n -- blobData :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- blobMimeType :: Maybe Text
  
instance Arbitrary CachedContent where
  arbitrary = sized genCachedContent

genCachedContent :: Int -> Gen CachedContent
genCachedContent n =
  CachedContent
    <$> arbitraryReducedMaybe n -- cachedContentTools :: Maybe [Tool]
    <*> arbitraryReducedMaybe n -- cachedContentDisplayName :: Maybe Text
    <*> arbitrary -- cachedContentModel :: Text
    <*> arbitraryReducedMaybe n -- cachedContentExpireTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- cachedContentUsageMetadata :: Maybe CachedContentUsageMetadata
    <*> arbitraryReducedMaybe n -- cachedContentName :: Maybe Text
    <*> arbitraryReducedMaybe n -- cachedContentContents :: Maybe [Content]
    <*> arbitraryReducedMaybe n -- cachedContentSystemInstruction :: Maybe Content
    <*> arbitraryReducedMaybe n -- cachedContentToolConfig :: Maybe ToolConfig
    <*> arbitraryReducedMaybe n -- cachedContentCreateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- cachedContentTtl :: Maybe Text
    <*> arbitraryReducedMaybe n -- cachedContentUpdateTime :: Maybe DateTime
  
instance Arbitrary CachedContentUsageMetadata where
  arbitrary = sized genCachedContentUsageMetadata

genCachedContentUsageMetadata :: Int -> Gen CachedContentUsageMetadata
genCachedContentUsageMetadata n =
  CachedContentUsageMetadata
    <$> arbitraryReducedMaybe n -- cachedContentUsageMetadataTotalTokenCount :: Maybe Int
  
instance Arbitrary Candidate where
  arbitrary = sized genCandidate

genCandidate :: Int -> Gen Candidate
genCandidate n =
  Candidate
    <$> arbitraryReducedMaybe n -- candidateCitationMetadata :: Maybe CitationMetadata
    <*> arbitraryReducedMaybe n -- candidateGroundingMetadata :: Maybe GroundingMetadata
    <*> arbitraryReducedMaybe n -- candidateUrlContextMetadata :: Maybe UrlContextMetadata
    <*> arbitraryReducedMaybe n -- candidateGroundingAttributions :: Maybe [GroundingAttribution]
    <*> arbitraryReducedMaybe n -- candidateLogprobsResult :: Maybe LogprobsResult
    <*> arbitraryReducedMaybe n -- candidateContent :: Maybe Content
    <*> arbitraryReducedMaybe n -- candidateAvgLogprobs :: Maybe Double
    <*> arbitraryReducedMaybe n -- candidateIndex :: Maybe Int
    <*> arbitraryReducedMaybe n -- candidateFinishReason :: Maybe E'FinishReason
    <*> arbitraryReducedMaybe n -- candidateSafetyRatings :: Maybe [SafetyRating]
    <*> arbitraryReducedMaybe n -- candidateTokenCount :: Maybe Int
  
instance Arbitrary Chunk where
  arbitrary = sized genChunk

genChunk :: Int -> Gen Chunk
genChunk n =
  Chunk
    <$> arbitraryReducedMaybe n -- chunkCreateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- chunkCustomMetadata :: Maybe [CustomMetadata]
    <*> arbitraryReduced n -- chunkData :: ChunkData
    <*> arbitraryReducedMaybe n -- chunkUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- chunkState :: Maybe E'State4
    <*> arbitraryReducedMaybe n -- chunkName :: Maybe Text
  
instance Arbitrary ChunkData where
  arbitrary = sized genChunkData

genChunkData :: Int -> Gen ChunkData
genChunkData n =
  ChunkData
    <$> arbitraryReducedMaybe n -- chunkDataStringValue :: Maybe Text
  
instance Arbitrary CitationMetadata where
  arbitrary = sized genCitationMetadata

genCitationMetadata :: Int -> Gen CitationMetadata
genCitationMetadata n =
  CitationMetadata
    <$> arbitraryReducedMaybe n -- citationMetadataCitationSources :: Maybe [CitationSource]
  
instance Arbitrary CitationSource where
  arbitrary = sized genCitationSource

genCitationSource :: Int -> Gen CitationSource
genCitationSource n =
  CitationSource
    <$> arbitraryReducedMaybe n -- citationSourceStartIndex :: Maybe Int
    <*> arbitraryReducedMaybe n -- citationSourceUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- citationSourceEndIndex :: Maybe Int
    <*> arbitraryReducedMaybe n -- citationSourceLicense :: Maybe Text
  
instance Arbitrary CodeExecutionResult where
  arbitrary = sized genCodeExecutionResult

genCodeExecutionResult :: Int -> Gen CodeExecutionResult
genCodeExecutionResult n =
  CodeExecutionResult
    <$> arbitrary -- codeExecutionResultOutcome :: E'Outcome
    <*> arbitraryReducedMaybe n -- codeExecutionResultOutput :: Maybe Text
  
instance Arbitrary Condition where
  arbitrary = sized genCondition

genCondition :: Int -> Gen Condition
genCondition n =
  Condition
    <$> arbitraryReducedMaybe n -- conditionNumericValue :: Maybe Float
    <*> arbitrary -- conditionOperation :: E'Operation
    <*> arbitraryReducedMaybe n -- conditionStringValue :: Maybe Text
  
instance Arbitrary Content where
  arbitrary = sized genContent

genContent :: Int -> Gen Content
genContent n =
  Content
    <$> arbitraryReducedMaybe n -- contentParts :: Maybe [Part]
    <*> arbitraryReducedMaybe n -- contentRole :: Maybe Text
  
instance Arbitrary ContentEmbedding where
  arbitrary = sized genContentEmbedding

genContentEmbedding :: Int -> Gen ContentEmbedding
genContentEmbedding n =
  ContentEmbedding
    <$> arbitraryReducedMaybe n -- contentEmbeddingValues :: Maybe [Float]
  
instance Arbitrary ContentFilter where
  arbitrary = sized genContentFilter

genContentFilter :: Int -> Gen ContentFilter
genContentFilter n =
  ContentFilter
    <$> arbitraryReducedMaybe n -- contentFilterReason :: Maybe E'Reason
    <*> arbitraryReducedMaybe n -- contentFilterMessage :: Maybe Text
  
instance Arbitrary Corpus where
  arbitrary = sized genCorpus

genCorpus :: Int -> Gen Corpus
genCorpus n =
  Corpus
    <$> arbitraryReducedMaybe n -- corpusUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- corpusCreateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- corpusDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- corpusName :: Maybe Text
  
instance Arbitrary CountMessageTokensRequest where
  arbitrary = sized genCountMessageTokensRequest

genCountMessageTokensRequest :: Int -> Gen CountMessageTokensRequest
genCountMessageTokensRequest n =
  CountMessageTokensRequest
    <$> arbitraryReduced n -- countMessageTokensRequestPrompt :: MessagePrompt
  
instance Arbitrary CountMessageTokensResponse where
  arbitrary = sized genCountMessageTokensResponse

genCountMessageTokensResponse :: Int -> Gen CountMessageTokensResponse
genCountMessageTokensResponse n =
  CountMessageTokensResponse
    <$> arbitraryReducedMaybe n -- countMessageTokensResponseTokenCount :: Maybe Int
  
instance Arbitrary CountTextTokensRequest where
  arbitrary = sized genCountTextTokensRequest

genCountTextTokensRequest :: Int -> Gen CountTextTokensRequest
genCountTextTokensRequest n =
  CountTextTokensRequest
    <$> arbitraryReduced n -- countTextTokensRequestPrompt :: TextPrompt
  
instance Arbitrary CountTextTokensResponse where
  arbitrary = sized genCountTextTokensResponse

genCountTextTokensResponse :: Int -> Gen CountTextTokensResponse
genCountTextTokensResponse n =
  CountTextTokensResponse
    <$> arbitraryReducedMaybe n -- countTextTokensResponseTokenCount :: Maybe Int
  
instance Arbitrary CountTokensRequest where
  arbitrary = sized genCountTokensRequest

genCountTokensRequest :: Int -> Gen CountTokensRequest
genCountTokensRequest n =
  CountTokensRequest
    <$> arbitraryReducedMaybe n -- countTokensRequestContents :: Maybe [Content]
    <*> arbitraryReducedMaybe n -- countTokensRequestGenerateContentRequest :: Maybe GenerateContentRequest
  
instance Arbitrary CountTokensResponse where
  arbitrary = sized genCountTokensResponse

genCountTokensResponse :: Int -> Gen CountTokensResponse
genCountTokensResponse n =
  CountTokensResponse
    <$> arbitraryReducedMaybe n -- countTokensResponseCacheTokensDetails :: Maybe [ModalityTokenCount]
    <*> arbitraryReducedMaybe n -- countTokensResponsePromptTokensDetails :: Maybe [ModalityTokenCount]
    <*> arbitraryReducedMaybe n -- countTokensResponseTotalTokens :: Maybe Int
    <*> arbitraryReducedMaybe n -- countTokensResponseCachedContentTokenCount :: Maybe Int
  
instance Arbitrary CreateChunkRequest where
  arbitrary = sized genCreateChunkRequest

genCreateChunkRequest :: Int -> Gen CreateChunkRequest
genCreateChunkRequest n =
  CreateChunkRequest
    <$> arbitrary -- createChunkRequestParent :: Text
    <*> arbitraryReduced n -- createChunkRequestChunk :: Chunk
  
instance Arbitrary CreateFileRequest where
  arbitrary = sized genCreateFileRequest

genCreateFileRequest :: Int -> Gen CreateFileRequest
genCreateFileRequest n =
  CreateFileRequest
    <$> arbitraryReducedMaybe n -- createFileRequestFile :: Maybe File
  
instance Arbitrary CreateFileResponse where
  arbitrary = sized genCreateFileResponse

genCreateFileResponse :: Int -> Gen CreateFileResponse
genCreateFileResponse n =
  CreateFileResponse
    <$> arbitraryReducedMaybe n -- createFileResponseFile :: Maybe File
  
instance Arbitrary CreateTunedModelMetadata where
  arbitrary = sized genCreateTunedModelMetadata

genCreateTunedModelMetadata :: Int -> Gen CreateTunedModelMetadata
genCreateTunedModelMetadata n =
  CreateTunedModelMetadata
    <$> arbitraryReducedMaybe n -- createTunedModelMetadataCompletedPercent :: Maybe Float
    <*> arbitraryReducedMaybe n -- createTunedModelMetadataCompletedSteps :: Maybe Int
    <*> arbitraryReducedMaybe n -- createTunedModelMetadataTotalSteps :: Maybe Int
    <*> arbitraryReducedMaybe n -- createTunedModelMetadataSnapshots :: Maybe [TuningSnapshot]
    <*> arbitraryReducedMaybe n -- createTunedModelMetadataTunedModel :: Maybe Text
  
instance Arbitrary CreateTunedModelOperation where
  arbitrary = sized genCreateTunedModelOperation

genCreateTunedModelOperation :: Int -> Gen CreateTunedModelOperation
genCreateTunedModelOperation n =
  CreateTunedModelOperation
    <$> arbitraryReducedMaybe n -- createTunedModelOperationDone :: Maybe Bool
    <*> arbitraryReducedMaybe n -- createTunedModelOperationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- createTunedModelOperationError :: Maybe Status
    <*> arbitraryReducedMaybe n -- createTunedModelOperationMetadata :: Maybe CreateTunedModelMetadata
    <*> arbitraryReducedMaybe n -- createTunedModelOperationResponse :: Maybe TunedModel
  
instance Arbitrary CustomMetadata where
  arbitrary = sized genCustomMetadata

genCustomMetadata :: Int -> Gen CustomMetadata
genCustomMetadata n =
  CustomMetadata
    <$> arbitraryReducedMaybe n -- customMetadataStringListValue :: Maybe StringList
    <*> arbitraryReducedMaybe n -- customMetadataStringValue :: Maybe Text
    <*> arbitrary -- customMetadataKey :: Text
    <*> arbitraryReducedMaybe n -- customMetadataNumericValue :: Maybe Float
  
instance Arbitrary Dataset where
  arbitrary = sized genDataset

genDataset :: Int -> Gen Dataset
genDataset n =
  Dataset
    <$> arbitraryReducedMaybe n -- datasetExamples :: Maybe TuningExamples
  
instance Arbitrary DeleteChunkRequest where
  arbitrary = sized genDeleteChunkRequest

genDeleteChunkRequest :: Int -> Gen DeleteChunkRequest
genDeleteChunkRequest n =
  DeleteChunkRequest
    <$> arbitrary -- deleteChunkRequestName :: Text
  
instance Arbitrary Document where
  arbitrary = sized genDocument

genDocument :: Int -> Gen Document
genDocument n =
  Document
    <$> arbitraryReducedMaybe n -- documentUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- documentName :: Maybe Text
    <*> arbitraryReducedMaybe n -- documentCustomMetadata :: Maybe [CustomMetadata]
    <*> arbitraryReducedMaybe n -- documentCreateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- documentDisplayName :: Maybe Text
  
instance Arbitrary DynamicRetrievalConfig where
  arbitrary = sized genDynamicRetrievalConfig

genDynamicRetrievalConfig :: Int -> Gen DynamicRetrievalConfig
genDynamicRetrievalConfig n =
  DynamicRetrievalConfig
    <$> arbitraryReducedMaybe n -- dynamicRetrievalConfigDynamicThreshold :: Maybe Float
    <*> arbitraryReducedMaybe n -- dynamicRetrievalConfigMode :: Maybe E'Mode
  
instance Arbitrary EmbedContentRequest where
  arbitrary = sized genEmbedContentRequest

genEmbedContentRequest :: Int -> Gen EmbedContentRequest
genEmbedContentRequest n =
  EmbedContentRequest
    <$> arbitraryReducedMaybe n -- embedContentRequestTaskType :: Maybe TaskType
    <*> arbitraryReduced n -- embedContentRequestContent :: Content
    <*> arbitraryReducedMaybe n -- embedContentRequestOutputDimensionality :: Maybe Int
    <*> arbitrary -- embedContentRequestModel :: Text
    <*> arbitraryReducedMaybe n -- embedContentRequestTitle :: Maybe Text
  
instance Arbitrary EmbedContentResponse where
  arbitrary = sized genEmbedContentResponse

genEmbedContentResponse :: Int -> Gen EmbedContentResponse
genEmbedContentResponse n =
  EmbedContentResponse
    <$> arbitraryReducedMaybe n -- embedContentResponseEmbedding :: Maybe ContentEmbedding
  
instance Arbitrary EmbedTextRequest where
  arbitrary = sized genEmbedTextRequest

genEmbedTextRequest :: Int -> Gen EmbedTextRequest
genEmbedTextRequest n =
  EmbedTextRequest
    <$> arbitraryReducedMaybe n -- embedTextRequestText :: Maybe Text
    <*> arbitrary -- embedTextRequestModel :: Text
  
instance Arbitrary EmbedTextResponse where
  arbitrary = sized genEmbedTextResponse

genEmbedTextResponse :: Int -> Gen EmbedTextResponse
genEmbedTextResponse n =
  EmbedTextResponse
    <$> arbitraryReducedMaybe n -- embedTextResponseEmbedding :: Maybe Embedding
  
instance Arbitrary Embedding where
  arbitrary = sized genEmbedding

genEmbedding :: Int -> Gen Embedding
genEmbedding n =
  Embedding
    <$> arbitraryReducedMaybe n -- embeddingValue :: Maybe [Float]
  
instance Arbitrary Example where
  arbitrary = sized genExample

genExample :: Int -> Gen Example
genExample n =
  Example
    <$> arbitraryReduced n -- exampleOutput :: Message
    <*> arbitraryReduced n -- exampleInput :: Message
  
instance Arbitrary ExecutableCode where
  arbitrary = sized genExecutableCode

genExecutableCode :: Int -> Gen ExecutableCode
genExecutableCode n =
  ExecutableCode
    <$> arbitrary -- executableCodeLanguage :: E'Language
    <*> arbitrary -- executableCodeCode :: Text
  
instance Arbitrary File where
  arbitrary = sized genFile

genFile :: Int -> Gen File
genFile n =
  File
    <$> arbitraryReducedMaybe n -- fileUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileExpirationTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- fileDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileVideoMetadata :: Maybe VideoFileMetadata
    <*> arbitraryReducedMaybe n -- fileState :: Maybe E'State
    <*> arbitraryReducedMaybe n -- fileSource :: Maybe E'Source
    <*> arbitraryReducedMaybe n -- fileMimeType :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileCreateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- fileError :: Maybe Status
    <*> arbitraryReducedMaybe n -- fileDownloadUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileSizeBytes :: Maybe Text
    <*> arbitraryReducedMaybe n -- fileSha256Hash :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- fileUpdateTime :: Maybe DateTime
  
instance Arbitrary FileData where
  arbitrary = sized genFileData

genFileData :: Int -> Gen FileData
genFileData n =
  FileData
    <$> arbitraryReducedMaybe n -- fileDataMimeType :: Maybe Text
    <*> arbitrary -- fileDataFileUri :: Text
  
instance Arbitrary FunctionCall where
  arbitrary = sized genFunctionCall

genFunctionCall :: Int -> Gen FunctionCall
genFunctionCall n =
  FunctionCall
    <$> arbitraryReducedMaybe n -- functionCallArgs :: Maybe (Map.Map String String)
    <*> arbitraryReducedMaybe n -- functionCallId :: Maybe Text
    <*> arbitrary -- functionCallName :: Text
  
instance Arbitrary FunctionCallingConfig where
  arbitrary = sized genFunctionCallingConfig

genFunctionCallingConfig :: Int -> Gen FunctionCallingConfig
genFunctionCallingConfig n =
  FunctionCallingConfig
    <$> arbitraryReducedMaybe n -- functionCallingConfigMode :: Maybe E'Mode2
    <*> arbitraryReducedMaybe n -- functionCallingConfigAllowedFunctionNames :: Maybe [Text]
  
instance Arbitrary FunctionDeclaration where
  arbitrary = sized genFunctionDeclaration

genFunctionDeclaration :: Int -> Gen FunctionDeclaration
genFunctionDeclaration n =
  FunctionDeclaration
    <$> arbitraryReducedMaybe n -- functionDeclarationParameters :: Maybe Schema
    <*> arbitrary -- functionDeclarationName :: Text
    <*> arbitraryReducedMaybe n -- functionDeclarationBehavior :: Maybe E'Behavior
    <*> arbitrary -- functionDeclarationDescription :: Text
    <*> arbitraryReducedMaybe n -- functionDeclarationResponse :: Maybe Schema
    <*> arbitraryReducedMaybe n -- functionDeclarationResponseJsonSchema :: Maybe String
    <*> arbitraryReducedMaybe n -- functionDeclarationParametersJsonSchema :: Maybe String
  
instance Arbitrary FunctionResponse where
  arbitrary = sized genFunctionResponse

genFunctionResponse :: Int -> Gen FunctionResponse
genFunctionResponse n =
  FunctionResponse
    <$> arbitraryReducedMaybe n -- functionResponseScheduling :: Maybe E'Scheduling
    <*> arbitraryReducedMaybe n -- functionResponseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- functionResponseWillContinue :: Maybe Bool
    <*> arbitrary -- functionResponseName :: Text
    <*> arbitrary -- functionResponseResponse :: (Map.Map String String)
  
instance Arbitrary GenerateAnswerRequest where
  arbitrary = sized genGenerateAnswerRequest

genGenerateAnswerRequest :: Int -> Gen GenerateAnswerRequest
genGenerateAnswerRequest n =
  GenerateAnswerRequest
    <$> arbitraryReducedMaybe n -- generateAnswerRequestSemanticRetriever :: Maybe SemanticRetrieverConfig
    <*> arbitraryReducedMaybe n -- generateAnswerRequestTemperature :: Maybe Float
    <*> arbitrary -- generateAnswerRequestAnswerStyle :: E'AnswerStyle
    <*> arbitraryReduced n -- generateAnswerRequestContents :: [Content]
    <*> arbitraryReducedMaybe n -- generateAnswerRequestSafetySettings :: Maybe [SafetySetting]
    <*> arbitraryReducedMaybe n -- generateAnswerRequestInlinePassages :: Maybe GroundingPassages
  
instance Arbitrary GenerateAnswerResponse where
  arbitrary = sized genGenerateAnswerResponse

genGenerateAnswerResponse :: Int -> Gen GenerateAnswerResponse
genGenerateAnswerResponse n =
  GenerateAnswerResponse
    <$> arbitraryReducedMaybe n -- generateAnswerResponseAnswer :: Maybe Candidate
    <*> arbitraryReducedMaybe n -- generateAnswerResponseInputFeedback :: Maybe InputFeedback
    <*> arbitraryReducedMaybe n -- generateAnswerResponseAnswerableProbability :: Maybe Float
  
instance Arbitrary GenerateContentRequest where
  arbitrary = sized genGenerateContentRequest

genGenerateContentRequest :: Int -> Gen GenerateContentRequest
genGenerateContentRequest n =
  GenerateContentRequest
    <$> arbitraryReducedMaybe n -- generateContentRequestToolConfig :: Maybe ToolConfig
    <*> arbitraryReducedMaybe n -- generateContentRequestTools :: Maybe [Tool]
    <*> arbitraryReduced n -- generateContentRequestContents :: [Content]
    <*> arbitraryReducedMaybe n -- generateContentRequestSystemInstruction :: Maybe Content
    <*> arbitraryReducedMaybe n -- generateContentRequestCachedContent :: Maybe Text
    <*> arbitraryReducedMaybe n -- generateContentRequestSafetySettings :: Maybe [SafetySetting]
    <*> arbitrary -- generateContentRequestModel :: Text
    <*> arbitraryReducedMaybe n -- generateContentRequestGenerationConfig :: Maybe GenerationConfig
  
instance Arbitrary GenerateContentResponse where
  arbitrary = sized genGenerateContentResponse

genGenerateContentResponse :: Int -> Gen GenerateContentResponse
genGenerateContentResponse n =
  GenerateContentResponse
    <$> arbitraryReducedMaybe n -- generateContentResponseCandidates :: Maybe [Candidate]
    <*> arbitraryReducedMaybe n -- generateContentResponseUsageMetadata :: Maybe UsageMetadata
    <*> arbitraryReducedMaybe n -- generateContentResponseModelVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- generateContentResponsePromptFeedback :: Maybe PromptFeedback
    <*> arbitraryReducedMaybe n -- generateContentResponseResponseId :: Maybe Text
  
instance Arbitrary GenerateMessageRequest where
  arbitrary = sized genGenerateMessageRequest

genGenerateMessageRequest :: Int -> Gen GenerateMessageRequest
genGenerateMessageRequest n =
  GenerateMessageRequest
    <$> arbitraryReducedMaybe n -- generateMessageRequestTemperature :: Maybe Float
    <*> arbitraryReducedMaybe n -- generateMessageRequestTopP :: Maybe Float
    <*> arbitraryReducedMaybe n -- generateMessageRequestCandidateCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- generateMessageRequestTopK :: Maybe Int
    <*> arbitraryReduced n -- generateMessageRequestPrompt :: MessagePrompt
  
instance Arbitrary GenerateMessageResponse where
  arbitrary = sized genGenerateMessageResponse

genGenerateMessageResponse :: Int -> Gen GenerateMessageResponse
genGenerateMessageResponse n =
  GenerateMessageResponse
    <$> arbitraryReducedMaybe n -- generateMessageResponseCandidates :: Maybe [Message]
    <*> arbitraryReducedMaybe n -- generateMessageResponseMessages :: Maybe [Message]
    <*> arbitraryReducedMaybe n -- generateMessageResponseFilters :: Maybe [ContentFilter]
  
instance Arbitrary GenerateTextRequest where
  arbitrary = sized genGenerateTextRequest

genGenerateTextRequest :: Int -> Gen GenerateTextRequest
genGenerateTextRequest n =
  GenerateTextRequest
    <$> arbitraryReducedMaybe n -- generateTextRequestStopSequences :: Maybe [Text]
    <*> arbitraryReduced n -- generateTextRequestPrompt :: TextPrompt
    <*> arbitraryReducedMaybe n -- generateTextRequestMaxOutputTokens :: Maybe Int
    <*> arbitraryReducedMaybe n -- generateTextRequestSafetySettings :: Maybe [SafetySetting]
    <*> arbitraryReducedMaybe n -- generateTextRequestTemperature :: Maybe Float
    <*> arbitraryReducedMaybe n -- generateTextRequestTopK :: Maybe Int
    <*> arbitraryReducedMaybe n -- generateTextRequestTopP :: Maybe Float
    <*> arbitraryReducedMaybe n -- generateTextRequestCandidateCount :: Maybe Int
  
instance Arbitrary GenerateTextResponse where
  arbitrary = sized genGenerateTextResponse

genGenerateTextResponse :: Int -> Gen GenerateTextResponse
genGenerateTextResponse n =
  GenerateTextResponse
    <$> arbitraryReducedMaybe n -- generateTextResponseSafetyFeedback :: Maybe [SafetyFeedback]
    <*> arbitraryReducedMaybe n -- generateTextResponseCandidates :: Maybe [TextCompletion]
    <*> arbitraryReducedMaybe n -- generateTextResponseFilters :: Maybe [ContentFilter]
  
instance Arbitrary GenerateVideoResponse where
  arbitrary = sized genGenerateVideoResponse

genGenerateVideoResponse :: Int -> Gen GenerateVideoResponse
genGenerateVideoResponse n =
  GenerateVideoResponse
    <$> arbitraryReducedMaybe n -- generateVideoResponseGeneratedSamples :: Maybe [Media]
    <*> arbitraryReducedMaybe n -- generateVideoResponseRaiMediaFilteredCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- generateVideoResponseRaiMediaFilteredReasons :: Maybe [Text]
  
instance Arbitrary GeneratedFile where
  arbitrary = sized genGeneratedFile

genGeneratedFile :: Int -> Gen GeneratedFile
genGeneratedFile n =
  GeneratedFile
    <$> arbitraryReducedMaybe n -- generatedFileError :: Maybe Status
    <*> arbitraryReducedMaybe n -- generatedFileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- generatedFileState :: Maybe E'State2
    <*> arbitraryReducedMaybe n -- generatedFileMimeType :: Maybe Text
  
instance Arbitrary GenerationConfig where
  arbitrary = sized genGenerationConfig

genGenerationConfig :: Int -> Gen GenerationConfig
genGenerationConfig n =
  GenerationConfig
    <$> arbitraryReducedMaybe n -- generationConfigResponseSchema :: Maybe Schema
    <*> arbitraryReducedMaybe n -- generationConfigThinkingConfig :: Maybe ThinkingConfig
    <*> arbitraryReducedMaybe n -- generationConfigLogprobs :: Maybe Int
    <*> arbitraryReducedMaybe n -- generationConfigMediaResolution :: Maybe E'MediaResolution
    <*> arbitraryReducedMaybe n -- generationConfigStopSequences :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- generationConfigSpeechConfig :: Maybe SpeechConfig
    <*> arbitraryReducedMaybe n -- generationConfigResponseJsonSchema :: Maybe String
    <*> arbitraryReducedMaybe n -- generationConfigPresencePenalty :: Maybe Float
    <*> arbitraryReducedMaybe n -- generationConfigTopP :: Maybe Float
    <*> arbitraryReducedMaybe n -- generationConfigTemperature :: Maybe Float
    <*> arbitraryReducedMaybe n -- generationConfigTopK :: Maybe Int
    <*> arbitraryReducedMaybe n -- generationConfigCandidateCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- generationConfigEnableEnhancedCivicAnswers :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generationConfigResponseLogprobs :: Maybe Bool
    <*> arbitraryReducedMaybe n -- generationConfigResponseModalities :: Maybe [E'ResponseModalities]
    <*> arbitraryReducedMaybe n -- generationConfigFrequencyPenalty :: Maybe Float
    <*> arbitraryReducedMaybe n -- generationConfigSeed :: Maybe Int
    <*> arbitraryReducedMaybe n -- generationConfigMaxOutputTokens :: Maybe Int
    <*> arbitraryReducedMaybe n -- generationConfigResponseMimeType :: Maybe Text
  
instance Arbitrary GoogleSearch where
  arbitrary = sized genGoogleSearch

genGoogleSearch :: Int -> Gen GoogleSearch
genGoogleSearch n =
  GoogleSearch
    <$> arbitraryReducedMaybe n -- googleSearchTimeRangeFilter :: Maybe Interval
  
instance Arbitrary GoogleSearchRetrieval where
  arbitrary = sized genGoogleSearchRetrieval

genGoogleSearchRetrieval :: Int -> Gen GoogleSearchRetrieval
genGoogleSearchRetrieval n =
  GoogleSearchRetrieval
    <$> arbitraryReducedMaybe n -- googleSearchRetrievalDynamicRetrievalConfig :: Maybe DynamicRetrievalConfig
  
instance Arbitrary GroundingAttribution where
  arbitrary = sized genGroundingAttribution

genGroundingAttribution :: Int -> Gen GroundingAttribution
genGroundingAttribution n =
  GroundingAttribution
    <$> arbitraryReducedMaybe n -- groundingAttributionSourceId :: Maybe AttributionSourceId
    <*> arbitraryReducedMaybe n -- groundingAttributionContent :: Maybe Content
  
instance Arbitrary GroundingChunk where
  arbitrary = sized genGroundingChunk

genGroundingChunk :: Int -> Gen GroundingChunk
genGroundingChunk n =
  GroundingChunk
    <$> arbitraryReducedMaybe n -- groundingChunkWeb :: Maybe Web
  
instance Arbitrary GroundingMetadata where
  arbitrary = sized genGroundingMetadata

genGroundingMetadata :: Int -> Gen GroundingMetadata
genGroundingMetadata n =
  GroundingMetadata
    <$> arbitraryReducedMaybe n -- groundingMetadataRetrievalMetadata :: Maybe RetrievalMetadata
    <*> arbitraryReducedMaybe n -- groundingMetadataWebSearchQueries :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- groundingMetadataGroundingChunks :: Maybe [GroundingChunk]
    <*> arbitraryReducedMaybe n -- groundingMetadataSearchEntryPoint :: Maybe SearchEntryPoint
    <*> arbitraryReducedMaybe n -- groundingMetadataGroundingSupports :: Maybe [GroundingSupport]
  
instance Arbitrary GroundingPassage where
  arbitrary = sized genGroundingPassage

genGroundingPassage :: Int -> Gen GroundingPassage
genGroundingPassage n =
  GroundingPassage
    <$> arbitraryReducedMaybe n -- groundingPassageContent :: Maybe Content
    <*> arbitraryReducedMaybe n -- groundingPassageId :: Maybe Text
  
instance Arbitrary GroundingPassageId where
  arbitrary = sized genGroundingPassageId

genGroundingPassageId :: Int -> Gen GroundingPassageId
genGroundingPassageId n =
  GroundingPassageId
    <$> arbitraryReducedMaybe n -- groundingPassageIdPassageId :: Maybe Text
    <*> arbitraryReducedMaybe n -- groundingPassageIdPartIndex :: Maybe Int
  
instance Arbitrary GroundingPassages where
  arbitrary = sized genGroundingPassages

genGroundingPassages :: Int -> Gen GroundingPassages
genGroundingPassages n =
  GroundingPassages
    <$> arbitraryReducedMaybe n -- groundingPassagesPassages :: Maybe [GroundingPassage]
  
instance Arbitrary GroundingSupport where
  arbitrary = sized genGroundingSupport

genGroundingSupport :: Int -> Gen GroundingSupport
genGroundingSupport n =
  GroundingSupport
    <$> arbitraryReducedMaybe n -- groundingSupportConfidenceScores :: Maybe [Float]
    <*> arbitraryReducedMaybe n -- groundingSupportGroundingChunkIndices :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- groundingSupportSegment :: Maybe Segment
  
instance Arbitrary Hyperparameters where
  arbitrary = sized genHyperparameters

genHyperparameters :: Int -> Gen Hyperparameters
genHyperparameters n =
  Hyperparameters
    <$> arbitraryReducedMaybe n -- hyperparametersEpochCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- hyperparametersLearningRate :: Maybe Float
    <*> arbitraryReducedMaybe n -- hyperparametersLearningRateMultiplier :: Maybe Float
    <*> arbitraryReducedMaybe n -- hyperparametersBatchSize :: Maybe Int
  
instance Arbitrary InputFeedback where
  arbitrary = sized genInputFeedback

genInputFeedback :: Int -> Gen InputFeedback
genInputFeedback n =
  InputFeedback
    <$> arbitraryReducedMaybe n -- inputFeedbackSafetyRatings :: Maybe [SafetyRating]
    <*> arbitraryReducedMaybe n -- inputFeedbackBlockReason :: Maybe E'BlockReason2
  
instance Arbitrary Interval where
  arbitrary = sized genInterval

genInterval :: Int -> Gen Interval
genInterval n =
  Interval
    <$> arbitraryReducedMaybe n -- intervalStartTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- intervalEndTime :: Maybe DateTime
  
instance Arbitrary ListCachedContentsResponse where
  arbitrary = sized genListCachedContentsResponse

genListCachedContentsResponse :: Int -> Gen ListCachedContentsResponse
genListCachedContentsResponse n =
  ListCachedContentsResponse
    <$> arbitraryReducedMaybe n -- listCachedContentsResponseNextPageToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- listCachedContentsResponseCachedContents :: Maybe [CachedContent]
  
instance Arbitrary ListChunksResponse where
  arbitrary = sized genListChunksResponse

genListChunksResponse :: Int -> Gen ListChunksResponse
genListChunksResponse n =
  ListChunksResponse
    <$> arbitraryReducedMaybe n -- listChunksResponseNextPageToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- listChunksResponseChunks :: Maybe [Chunk]
  
instance Arbitrary ListCorporaResponse where
  arbitrary = sized genListCorporaResponse

genListCorporaResponse :: Int -> Gen ListCorporaResponse
genListCorporaResponse n =
  ListCorporaResponse
    <$> arbitraryReducedMaybe n -- listCorporaResponseCorpora :: Maybe [Corpus]
    <*> arbitraryReducedMaybe n -- listCorporaResponseNextPageToken :: Maybe Text
  
instance Arbitrary ListDocumentsResponse where
  arbitrary = sized genListDocumentsResponse

genListDocumentsResponse :: Int -> Gen ListDocumentsResponse
genListDocumentsResponse n =
  ListDocumentsResponse
    <$> arbitraryReducedMaybe n -- listDocumentsResponseNextPageToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- listDocumentsResponseDocuments :: Maybe [Document]
  
instance Arbitrary ListFilesResponse where
  arbitrary = sized genListFilesResponse

genListFilesResponse :: Int -> Gen ListFilesResponse
genListFilesResponse n =
  ListFilesResponse
    <$> arbitraryReducedMaybe n -- listFilesResponseNextPageToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- listFilesResponseFiles :: Maybe [File]
  
instance Arbitrary ListGeneratedFilesResponse where
  arbitrary = sized genListGeneratedFilesResponse

genListGeneratedFilesResponse :: Int -> Gen ListGeneratedFilesResponse
genListGeneratedFilesResponse n =
  ListGeneratedFilesResponse
    <$> arbitraryReducedMaybe n -- listGeneratedFilesResponseNextPageToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- listGeneratedFilesResponseGeneratedFiles :: Maybe [GeneratedFile]
  
instance Arbitrary ListModelsResponse where
  arbitrary = sized genListModelsResponse

genListModelsResponse :: Int -> Gen ListModelsResponse
genListModelsResponse n =
  ListModelsResponse
    <$> arbitraryReducedMaybe n -- listModelsResponseModels :: Maybe [Model]
    <*> arbitraryReducedMaybe n -- listModelsResponseNextPageToken :: Maybe Text
  
instance Arbitrary ListOperationsResponse where
  arbitrary = sized genListOperationsResponse

genListOperationsResponse :: Int -> Gen ListOperationsResponse
genListOperationsResponse n =
  ListOperationsResponse
    <$> arbitraryReducedMaybe n -- listOperationsResponseNextPageToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- listOperationsResponseOperations :: Maybe [Operation]
  
instance Arbitrary ListPermissionsResponse where
  arbitrary = sized genListPermissionsResponse

genListPermissionsResponse :: Int -> Gen ListPermissionsResponse
genListPermissionsResponse n =
  ListPermissionsResponse
    <$> arbitraryReducedMaybe n -- listPermissionsResponsePermissions :: Maybe [Permission]
    <*> arbitraryReducedMaybe n -- listPermissionsResponseNextPageToken :: Maybe Text
  
instance Arbitrary ListTunedModelsResponse where
  arbitrary = sized genListTunedModelsResponse

genListTunedModelsResponse :: Int -> Gen ListTunedModelsResponse
genListTunedModelsResponse n =
  ListTunedModelsResponse
    <$> arbitraryReducedMaybe n -- listTunedModelsResponseNextPageToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- listTunedModelsResponseTunedModels :: Maybe [TunedModel]
  
instance Arbitrary LogprobsResult where
  arbitrary = sized genLogprobsResult

genLogprobsResult :: Int -> Gen LogprobsResult
genLogprobsResult n =
  LogprobsResult
    <$> arbitraryReducedMaybe n -- logprobsResultChosenCandidates :: Maybe [LogprobsResultCandidate]
    <*> arbitraryReducedMaybe n -- logprobsResultTopCandidates :: Maybe [TopCandidates]
  
instance Arbitrary LogprobsResultCandidate where
  arbitrary = sized genLogprobsResultCandidate

genLogprobsResultCandidate :: Int -> Gen LogprobsResultCandidate
genLogprobsResultCandidate n =
  LogprobsResultCandidate
    <$> arbitraryReducedMaybe n -- logprobsResultCandidateLogProbability :: Maybe Float
    <*> arbitraryReducedMaybe n -- logprobsResultCandidateTokenId :: Maybe Int
    <*> arbitraryReducedMaybe n -- logprobsResultCandidateToken :: Maybe Text
  
instance Arbitrary Media where
  arbitrary = sized genMedia

genMedia :: Int -> Gen Media
genMedia n =
  Media
    <$> arbitraryReducedMaybe n -- mediaVideo :: Maybe Video
  
instance Arbitrary Message where
  arbitrary = sized genMessage

genMessage :: Int -> Gen Message
genMessage n =
  Message
    <$> arbitraryReducedMaybe n -- messageCitationMetadata :: Maybe CitationMetadata
    <*> arbitraryReducedMaybe n -- messageAuthor :: Maybe Text
    <*> arbitrary -- messageContent :: Text
  
instance Arbitrary MessagePrompt where
  arbitrary = sized genMessagePrompt

genMessagePrompt :: Int -> Gen MessagePrompt
genMessagePrompt n =
  MessagePrompt
    <$> arbitraryReducedMaybe n -- messagePromptContext :: Maybe Text
    <*> arbitraryReduced n -- messagePromptMessages :: [Message]
    <*> arbitraryReducedMaybe n -- messagePromptExamples :: Maybe [Example]
  
instance Arbitrary MetadataFilter where
  arbitrary = sized genMetadataFilter

genMetadataFilter :: Int -> Gen MetadataFilter
genMetadataFilter n =
  MetadataFilter
    <$> arbitraryReduced n -- metadataFilterConditions :: [Condition]
    <*> arbitrary -- metadataFilterKey :: Text
  
instance Arbitrary ModalityTokenCount where
  arbitrary = sized genModalityTokenCount

genModalityTokenCount :: Int -> Gen ModalityTokenCount
genModalityTokenCount n =
  ModalityTokenCount
    <$> arbitraryReducedMaybe n -- modalityTokenCountTokenCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- modalityTokenCountModality :: Maybe Modality
  
instance Arbitrary Model where
  arbitrary = sized genModel

genModel :: Int -> Gen Model
genModel n =
  Model
    <$> arbitraryReducedMaybe n -- modelTopK :: Maybe Int
    <*> arbitrary -- modelName :: Text
    <*> arbitrary -- modelBaseModelId :: Text
    <*> arbitrary -- modelVersion :: Text
    <*> arbitraryReducedMaybe n -- modelInputTokenLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelTopP :: Maybe Float
    <*> arbitraryReducedMaybe n -- modelSupportedGenerationMethods :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- modelTemperature :: Maybe Float
    <*> arbitraryReducedMaybe n -- modelDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelMaxTemperature :: Maybe Float
    <*> arbitraryReducedMaybe n -- modelOutputTokenLimit :: Maybe Int
  
instance Arbitrary MultiSpeakerVoiceConfig where
  arbitrary = sized genMultiSpeakerVoiceConfig

genMultiSpeakerVoiceConfig :: Int -> Gen MultiSpeakerVoiceConfig
genMultiSpeakerVoiceConfig n =
  MultiSpeakerVoiceConfig
    <$> arbitraryReduced n -- multiSpeakerVoiceConfigSpeakerVoiceConfigs :: [SpeakerVoiceConfig]
  
instance Arbitrary Operation where
  arbitrary = sized genOperation

genOperation :: Int -> Gen Operation
genOperation n =
  Operation
    <$> arbitraryReducedMaybe n -- operationDone :: Maybe Bool
    <*> arbitraryReducedMaybe n -- operationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- operationError :: Maybe Status
    <*> arbitraryReducedMaybe n -- operationMetadata :: Maybe (Map.Map String String)
    <*> arbitraryReducedMaybe n -- operationResponse :: Maybe (Map.Map String String)
  
instance Arbitrary Part where
  arbitrary = sized genPart

genPart :: Int -> Gen Part
genPart n =
  Part
    <$> arbitraryReducedMaybe n -- partInlineData :: Maybe Blob
    <*> arbitraryReducedMaybe n -- partFunctionResponse :: Maybe FunctionResponse
    <*> arbitraryReducedMaybe n -- partCodeExecutionResult :: Maybe CodeExecutionResult
    <*> arbitraryReducedMaybe n -- partFileData :: Maybe FileData
    <*> arbitraryReducedMaybe n -- partExecutableCode :: Maybe ExecutableCode
    <*> arbitraryReducedMaybe n -- partVideoMetadata :: Maybe VideoMetadata
    <*> arbitraryReducedMaybe n -- partThought :: Maybe Bool
    <*> arbitraryReducedMaybe n -- partText :: Maybe Text
    <*> arbitraryReducedMaybe n -- partThoughtSignature :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- partFunctionCall :: Maybe FunctionCall
  
instance Arbitrary Permission where
  arbitrary = sized genPermission

genPermission :: Int -> Gen Permission
genPermission n =
  Permission
    <$> arbitraryReducedMaybe n -- permissionName :: Maybe Text
    <*> arbitraryReducedMaybe n -- permissionGranteeType :: Maybe E'GranteeType
    <*> arbitrary -- permissionRole :: E'Role
    <*> arbitraryReducedMaybe n -- permissionEmailAddress :: Maybe Text
  
instance Arbitrary PrebuiltVoiceConfig where
  arbitrary = sized genPrebuiltVoiceConfig

genPrebuiltVoiceConfig :: Int -> Gen PrebuiltVoiceConfig
genPrebuiltVoiceConfig n =
  PrebuiltVoiceConfig
    <$> arbitraryReducedMaybe n -- prebuiltVoiceConfigVoiceName :: Maybe Text
  
instance Arbitrary PredictLongRunningOperation where
  arbitrary = sized genPredictLongRunningOperation

genPredictLongRunningOperation :: Int -> Gen PredictLongRunningOperation
genPredictLongRunningOperation n =
  PredictLongRunningOperation
    <$> arbitraryReducedMaybe n -- predictLongRunningOperationDone :: Maybe Bool
    <*> arbitraryReducedMaybe n -- predictLongRunningOperationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- predictLongRunningOperationError :: Maybe Status
    <*> arbitraryReducedMaybeValue n -- predictLongRunningOperationMetadata :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- predictLongRunningOperationResponse :: Maybe PredictLongRunningResponse
  
instance Arbitrary PredictLongRunningRequest where
  arbitrary = sized genPredictLongRunningRequest

genPredictLongRunningRequest :: Int -> Gen PredictLongRunningRequest
genPredictLongRunningRequest n =
  PredictLongRunningRequest
    <$> arbitraryReducedMaybe n -- predictLongRunningRequestParameters :: Maybe String
    <*> arbitrary -- predictLongRunningRequestInstances :: [String]
  
instance Arbitrary PredictLongRunningResponse where
  arbitrary = sized genPredictLongRunningResponse

genPredictLongRunningResponse :: Int -> Gen PredictLongRunningResponse
genPredictLongRunningResponse n =
  PredictLongRunningResponse
    <$> arbitraryReducedMaybe n -- predictLongRunningResponseGenerateVideoResponse :: Maybe GenerateVideoResponse
  
instance Arbitrary PredictRequest where
  arbitrary = sized genPredictRequest

genPredictRequest :: Int -> Gen PredictRequest
genPredictRequest n =
  PredictRequest
    <$> arbitrary -- predictRequestInstances :: [String]
    <*> arbitraryReducedMaybe n -- predictRequestParameters :: Maybe String
  
instance Arbitrary PredictResponse where
  arbitrary = sized genPredictResponse

genPredictResponse :: Int -> Gen PredictResponse
genPredictResponse n =
  PredictResponse
    <$> arbitraryReducedMaybe n -- predictResponsePredictions :: Maybe [String]
  
instance Arbitrary PromptFeedback where
  arbitrary = sized genPromptFeedback

genPromptFeedback :: Int -> Gen PromptFeedback
genPromptFeedback n =
  PromptFeedback
    <$> arbitraryReducedMaybe n -- promptFeedbackBlockReason :: Maybe E'BlockReason
    <*> arbitraryReducedMaybe n -- promptFeedbackSafetyRatings :: Maybe [SafetyRating]
  
instance Arbitrary QueryCorpusRequest where
  arbitrary = sized genQueryCorpusRequest

genQueryCorpusRequest :: Int -> Gen QueryCorpusRequest
genQueryCorpusRequest n =
  QueryCorpusRequest
    <$> arbitraryReducedMaybe n -- queryCorpusRequestMetadataFilters :: Maybe [MetadataFilter]
    <*> arbitrary -- queryCorpusRequestQuery :: Text
    <*> arbitraryReducedMaybe n -- queryCorpusRequestResultsCount :: Maybe Int
  
instance Arbitrary QueryCorpusResponse where
  arbitrary = sized genQueryCorpusResponse

genQueryCorpusResponse :: Int -> Gen QueryCorpusResponse
genQueryCorpusResponse n =
  QueryCorpusResponse
    <$> arbitraryReducedMaybe n -- queryCorpusResponseRelevantChunks :: Maybe [RelevantChunk]
  
instance Arbitrary QueryDocumentRequest where
  arbitrary = sized genQueryDocumentRequest

genQueryDocumentRequest :: Int -> Gen QueryDocumentRequest
genQueryDocumentRequest n =
  QueryDocumentRequest
    <$> arbitrary -- queryDocumentRequestQuery :: Text
    <*> arbitraryReducedMaybe n -- queryDocumentRequestResultsCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- queryDocumentRequestMetadataFilters :: Maybe [MetadataFilter]
  
instance Arbitrary QueryDocumentResponse where
  arbitrary = sized genQueryDocumentResponse

genQueryDocumentResponse :: Int -> Gen QueryDocumentResponse
genQueryDocumentResponse n =
  QueryDocumentResponse
    <$> arbitraryReducedMaybe n -- queryDocumentResponseRelevantChunks :: Maybe [RelevantChunk]
  
instance Arbitrary RelevantChunk where
  arbitrary = sized genRelevantChunk

genRelevantChunk :: Int -> Gen RelevantChunk
genRelevantChunk n =
  RelevantChunk
    <$> arbitraryReducedMaybe n -- relevantChunkChunk :: Maybe Chunk
    <*> arbitraryReducedMaybe n -- relevantChunkChunkRelevanceScore :: Maybe Float
  
instance Arbitrary RetrievalMetadata where
  arbitrary = sized genRetrievalMetadata

genRetrievalMetadata :: Int -> Gen RetrievalMetadata
genRetrievalMetadata n =
  RetrievalMetadata
    <$> arbitraryReducedMaybe n -- retrievalMetadataGoogleSearchDynamicRetrievalScore :: Maybe Float
  
instance Arbitrary SafetyFeedback where
  arbitrary = sized genSafetyFeedback

genSafetyFeedback :: Int -> Gen SafetyFeedback
genSafetyFeedback n =
  SafetyFeedback
    <$> arbitraryReducedMaybe n -- safetyFeedbackSetting :: Maybe SafetySetting
    <*> arbitraryReducedMaybe n -- safetyFeedbackRating :: Maybe SafetyRating
  
instance Arbitrary SafetyRating where
  arbitrary = sized genSafetyRating

genSafetyRating :: Int -> Gen SafetyRating
genSafetyRating n =
  SafetyRating
    <$> arbitraryReduced n -- safetyRatingCategory :: HarmCategory
    <*> arbitraryReducedMaybe n -- safetyRatingBlocked :: Maybe Bool
    <*> arbitrary -- safetyRatingProbability :: E'Probability
  
instance Arbitrary SafetySetting where
  arbitrary = sized genSafetySetting

genSafetySetting :: Int -> Gen SafetySetting
genSafetySetting n =
  SafetySetting
    <$> arbitrary -- safetySettingThreshold :: E'Threshold
    <*> arbitraryReduced n -- safetySettingCategory :: HarmCategory
  
instance Arbitrary Schema where
  arbitrary = sized genSchema

genSchema :: Int -> Gen Schema
genSchema n =
  Schema
    <$> arbitraryReducedMaybe n -- schemaItems :: Maybe Schema
    <*> arbitraryReducedMaybe n -- schemaAnyOf :: Maybe [Schema]
    <*> arbitraryReducedMaybe n -- schemaMinLength :: Maybe Text
    <*> arbitraryReducedMaybe n -- schemaMaximum :: Maybe Double
    <*> arbitraryReducedMaybe n -- schemaPropertyOrdering :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- schemaNullable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- schemaRequired :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- schemaMinProperties :: Maybe Text
    <*> arbitraryReducedMaybe n -- schemaMaxItems :: Maybe Text
    <*> arbitraryReducedMaybe n -- schemaExample :: Maybe String
    <*> arbitraryReducedMaybe n -- schemaTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- schemaMinItems :: Maybe Text
    <*> arbitraryReducedMaybe n -- schemaDescription :: Maybe Text
    <*> arbitraryReduced n -- schemaType :: ModelType
    <*> arbitraryReducedMaybe n -- schemaDefault :: Maybe String
    <*> arbitraryReducedMaybe n -- schemaMinimum :: Maybe Double
    <*> arbitraryReducedMaybe n -- schemaPattern :: Maybe Text
    <*> arbitraryReducedMaybe n -- schemaProperties :: Maybe (Map.Map String Schema)
    <*> arbitraryReducedMaybe n -- schemaMaxProperties :: Maybe Text
    <*> arbitraryReducedMaybe n -- schemaFormat :: Maybe Text
    <*> arbitraryReducedMaybe n -- schemaEnum :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- schemaMaxLength :: Maybe Text
  
instance Arbitrary SearchEntryPoint where
  arbitrary = sized genSearchEntryPoint

genSearchEntryPoint :: Int -> Gen SearchEntryPoint
genSearchEntryPoint n =
  SearchEntryPoint
    <$> arbitraryReducedMaybe n -- searchEntryPointSdkBlob :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- searchEntryPointRenderedContent :: Maybe Text
  
instance Arbitrary Segment where
  arbitrary = sized genSegment

genSegment :: Int -> Gen Segment
genSegment n =
  Segment
    <$> arbitraryReducedMaybe n -- segmentPartIndex :: Maybe Int
    <*> arbitraryReducedMaybe n -- segmentStartIndex :: Maybe Int
    <*> arbitraryReducedMaybe n -- segmentText :: Maybe Text
    <*> arbitraryReducedMaybe n -- segmentEndIndex :: Maybe Int
  
instance Arbitrary SemanticRetrieverChunk where
  arbitrary = sized genSemanticRetrieverChunk

genSemanticRetrieverChunk :: Int -> Gen SemanticRetrieverChunk
genSemanticRetrieverChunk n =
  SemanticRetrieverChunk
    <$> arbitraryReducedMaybe n -- semanticRetrieverChunkChunk :: Maybe Text
    <*> arbitraryReducedMaybe n -- semanticRetrieverChunkSource :: Maybe Text
  
instance Arbitrary SemanticRetrieverConfig where
  arbitrary = sized genSemanticRetrieverConfig

genSemanticRetrieverConfig :: Int -> Gen SemanticRetrieverConfig
genSemanticRetrieverConfig n =
  SemanticRetrieverConfig
    <$> arbitrary -- semanticRetrieverConfigSource :: Text
    <*> arbitraryReduced n -- semanticRetrieverConfigQuery :: Content
    <*> arbitraryReducedMaybe n -- semanticRetrieverConfigMaxChunksCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- semanticRetrieverConfigMetadataFilters :: Maybe [MetadataFilter]
    <*> arbitraryReducedMaybe n -- semanticRetrieverConfigMinimumRelevanceScore :: Maybe Float
  
instance Arbitrary SpeakerVoiceConfig where
  arbitrary = sized genSpeakerVoiceConfig

genSpeakerVoiceConfig :: Int -> Gen SpeakerVoiceConfig
genSpeakerVoiceConfig n =
  SpeakerVoiceConfig
    <$> arbitraryReduced n -- speakerVoiceConfigVoiceConfig :: VoiceConfig
    <*> arbitrary -- speakerVoiceConfigSpeaker :: Text
  
instance Arbitrary SpeechConfig where
  arbitrary = sized genSpeechConfig

genSpeechConfig :: Int -> Gen SpeechConfig
genSpeechConfig n =
  SpeechConfig
    <$> arbitraryReducedMaybe n -- speechConfigVoiceConfig :: Maybe VoiceConfig
    <*> arbitraryReducedMaybe n -- speechConfigLanguageCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- speechConfigMultiSpeakerVoiceConfig :: Maybe MultiSpeakerVoiceConfig
  
instance Arbitrary Status where
  arbitrary = sized genStatus

genStatus :: Int -> Gen Status
genStatus n =
  Status
    <$> arbitraryReducedMaybe n -- statusCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- statusDetails :: Maybe [(Map.Map String String)]
    <*> arbitraryReducedMaybe n -- statusMessage :: Maybe Text
  
instance Arbitrary StringList where
  arbitrary = sized genStringList

genStringList :: Int -> Gen StringList
genStringList n =
  StringList
    <$> arbitraryReducedMaybe n -- stringListValues :: Maybe [Text]
  
instance Arbitrary TextCompletion where
  arbitrary = sized genTextCompletion

genTextCompletion :: Int -> Gen TextCompletion
genTextCompletion n =
  TextCompletion
    <$> arbitraryReducedMaybe n -- textCompletionSafetyRatings :: Maybe [SafetyRating]
    <*> arbitraryReducedMaybe n -- textCompletionOutput :: Maybe Text
    <*> arbitraryReducedMaybe n -- textCompletionCitationMetadata :: Maybe CitationMetadata
  
instance Arbitrary TextPrompt where
  arbitrary = sized genTextPrompt

genTextPrompt :: Int -> Gen TextPrompt
genTextPrompt n =
  TextPrompt
    <$> arbitrary -- textPromptText :: Text
  
instance Arbitrary ThinkingConfig where
  arbitrary = sized genThinkingConfig

genThinkingConfig :: Int -> Gen ThinkingConfig
genThinkingConfig n =
  ThinkingConfig
    <$> arbitraryReducedMaybe n -- thinkingConfigThinkingBudget :: Maybe Int
    <*> arbitraryReducedMaybe n -- thinkingConfigIncludeThoughts :: Maybe Bool
  
instance Arbitrary Tool where
  arbitrary = sized genTool

genTool :: Int -> Gen Tool
genTool n =
  Tool
    <$> arbitraryReducedMaybe n -- toolFunctionDeclarations :: Maybe [FunctionDeclaration]
    <*> arbitraryReducedMaybe n -- toolGoogleSearchRetrieval :: Maybe GoogleSearchRetrieval
    <*> arbitraryReducedMaybe n -- toolGoogleSearch :: Maybe GoogleSearch
    <*> arbitraryReducedMaybeValue n -- toolCodeExecution :: Maybe A.Value
    <*> arbitraryReducedMaybeValue n -- toolUrlContext :: Maybe A.Value
  
instance Arbitrary ToolConfig where
  arbitrary = sized genToolConfig

genToolConfig :: Int -> Gen ToolConfig
genToolConfig n =
  ToolConfig
    <$> arbitraryReducedMaybe n -- toolConfigFunctionCallingConfig :: Maybe FunctionCallingConfig
  
instance Arbitrary TopCandidates where
  arbitrary = sized genTopCandidates

genTopCandidates :: Int -> Gen TopCandidates
genTopCandidates n =
  TopCandidates
    <$> arbitraryReducedMaybe n -- topCandidatesCandidates :: Maybe [LogprobsResultCandidate]
  
instance Arbitrary TransferOwnershipRequest where
  arbitrary = sized genTransferOwnershipRequest

genTransferOwnershipRequest :: Int -> Gen TransferOwnershipRequest
genTransferOwnershipRequest n =
  TransferOwnershipRequest
    <$> arbitrary -- transferOwnershipRequestEmailAddress :: Text
  
instance Arbitrary TunedModel where
  arbitrary = sized genTunedModel

genTunedModel :: Int -> Gen TunedModel
genTunedModel n =
  TunedModel
    <$> arbitraryReducedMaybe n -- tunedModelUpdateTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- tunedModelName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tunedModelCreateTime :: Maybe DateTime
    <*> arbitraryReduced n -- tunedModelTuningTask :: TuningTask
    <*> arbitraryReducedMaybe n -- tunedModelTunedModelSource :: Maybe TunedModelSource
    <*> arbitraryReducedMaybe n -- tunedModelBaseModel :: Maybe Text
    <*> arbitraryReducedMaybe n -- tunedModelReaderProjectNumbers :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- tunedModelDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- tunedModelTemperature :: Maybe Float
    <*> arbitraryReducedMaybe n -- tunedModelDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- tunedModelTopP :: Maybe Float
    <*> arbitraryReducedMaybe n -- tunedModelTopK :: Maybe Int
    <*> arbitraryReducedMaybe n -- tunedModelState :: Maybe E'State3
  
instance Arbitrary TunedModelSource where
  arbitrary = sized genTunedModelSource

genTunedModelSource :: Int -> Gen TunedModelSource
genTunedModelSource n =
  TunedModelSource
    <$> arbitraryReducedMaybe n -- tunedModelSourceTunedModel :: Maybe Text
    <*> arbitraryReducedMaybe n -- tunedModelSourceBaseModel :: Maybe Text
  
instance Arbitrary TuningExample where
  arbitrary = sized genTuningExample

genTuningExample :: Int -> Gen TuningExample
genTuningExample n =
  TuningExample
    <$> arbitraryReducedMaybe n -- tuningExampleTextInput :: Maybe Text
    <*> arbitrary -- tuningExampleOutput :: Text
  
instance Arbitrary TuningExamples where
  arbitrary = sized genTuningExamples

genTuningExamples :: Int -> Gen TuningExamples
genTuningExamples n =
  TuningExamples
    <$> arbitraryReducedMaybe n -- tuningExamplesExamples :: Maybe [TuningExample]
  
instance Arbitrary TuningSnapshot where
  arbitrary = sized genTuningSnapshot

genTuningSnapshot :: Int -> Gen TuningSnapshot
genTuningSnapshot n =
  TuningSnapshot
    <$> arbitraryReducedMaybe n -- tuningSnapshotMeanLoss :: Maybe Float
    <*> arbitraryReducedMaybe n -- tuningSnapshotComputeTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- tuningSnapshotStep :: Maybe Int
    <*> arbitraryReducedMaybe n -- tuningSnapshotEpoch :: Maybe Int
  
instance Arbitrary TuningTask where
  arbitrary = sized genTuningTask

genTuningTask :: Int -> Gen TuningTask
genTuningTask n =
  TuningTask
    <$> arbitraryReducedMaybe n -- tuningTaskStartTime :: Maybe DateTime
    <*> arbitraryReduced n -- tuningTaskTrainingData :: Dataset
    <*> arbitraryReducedMaybe n -- tuningTaskHyperparameters :: Maybe Hyperparameters
    <*> arbitraryReducedMaybe n -- tuningTaskCompleteTime :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- tuningTaskSnapshots :: Maybe [TuningSnapshot]
  
instance Arbitrary UpdateChunkRequest where
  arbitrary = sized genUpdateChunkRequest

genUpdateChunkRequest :: Int -> Gen UpdateChunkRequest
genUpdateChunkRequest n =
  UpdateChunkRequest
    <$> arbitrary -- updateChunkRequestUpdateMask :: Text
    <*> arbitraryReduced n -- updateChunkRequestChunk :: Chunk
  
instance Arbitrary UrlContextMetadata where
  arbitrary = sized genUrlContextMetadata

genUrlContextMetadata :: Int -> Gen UrlContextMetadata
genUrlContextMetadata n =
  UrlContextMetadata
    <$> arbitraryReducedMaybe n -- urlContextMetadataUrlMetadata :: Maybe [UrlMetadata]
  
instance Arbitrary UrlMetadata where
  arbitrary = sized genUrlMetadata

genUrlMetadata :: Int -> Gen UrlMetadata
genUrlMetadata n =
  UrlMetadata
    <$> arbitraryReducedMaybe n -- urlMetadataRetrievedUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- urlMetadataUrlRetrievalStatus :: Maybe E'UrlRetrievalStatus
  
instance Arbitrary UsageMetadata where
  arbitrary = sized genUsageMetadata

genUsageMetadata :: Int -> Gen UsageMetadata
genUsageMetadata n =
  UsageMetadata
    <$> arbitraryReducedMaybe n -- usageMetadataCandidatesTokensDetails :: Maybe [ModalityTokenCount]
    <*> arbitraryReducedMaybe n -- usageMetadataThoughtsTokenCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- usageMetadataToolUsePromptTokenCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- usageMetadataCachedContentTokenCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- usageMetadataPromptTokenCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- usageMetadataCandidatesTokenCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- usageMetadataPromptTokensDetails :: Maybe [ModalityTokenCount]
    <*> arbitraryReducedMaybe n -- usageMetadataTotalTokenCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- usageMetadataCacheTokensDetails :: Maybe [ModalityTokenCount]
    <*> arbitraryReducedMaybe n -- usageMetadataToolUsePromptTokensDetails :: Maybe [ModalityTokenCount]
  
instance Arbitrary Video where
  arbitrary = sized genVideo

genVideo :: Int -> Gen Video
genVideo n =
  Video
    <$> arbitraryReducedMaybe n -- videoVideo :: Maybe ByteArray
    <*> arbitraryReducedMaybe n -- videoUri :: Maybe Text
  
instance Arbitrary VideoFileMetadata where
  arbitrary = sized genVideoFileMetadata

genVideoFileMetadata :: Int -> Gen VideoFileMetadata
genVideoFileMetadata n =
  VideoFileMetadata
    <$> arbitraryReducedMaybe n -- videoFileMetadataVideoDuration :: Maybe Text
  
instance Arbitrary VideoMetadata where
  arbitrary = sized genVideoMetadata

genVideoMetadata :: Int -> Gen VideoMetadata
genVideoMetadata n =
  VideoMetadata
    <$> arbitraryReducedMaybe n -- videoMetadataEndOffset :: Maybe Text
    <*> arbitraryReducedMaybe n -- videoMetadataStartOffset :: Maybe Text
    <*> arbitraryReducedMaybe n -- videoMetadataFps :: Maybe Double
  
instance Arbitrary VoiceConfig where
  arbitrary = sized genVoiceConfig

genVoiceConfig :: Int -> Gen VoiceConfig
genVoiceConfig n =
  VoiceConfig
    <$> arbitraryReducedMaybe n -- voiceConfigPrebuiltVoiceConfig :: Maybe PrebuiltVoiceConfig
  
instance Arbitrary Web where
  arbitrary = sized genWeb

genWeb :: Int -> Gen Web
genWeb n =
  Web
    <$> arbitraryReducedMaybe n -- webTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- webUri :: Maybe Text
  



instance Arbitrary E'Alt where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AnswerStyle where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Behavior where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'BlockReason where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'BlockReason2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FinishReason where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'GranteeType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Language where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'MediaResolution where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Mode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Mode2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Operation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Outcome where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Probability where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Reason where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ResponseModalities where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Role where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Scheduling where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Source where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Threshold where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'UrlRetrievalStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Xgafv where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary HarmCategory where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Modality where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TaskType where
  arbitrary = arbitraryBoundedEnum

