{-# LANGUAGE OverloadedStrings #-}
-- | Generate sql queries from JSON. You could use this to query database directly from frontend. Basic sql queries are supported and the resulting sql query is returned in json format. 


module Data.Serviette
  ( rawSqlStr
  , SqlQuery(..)
  , SqlResponse(..)
  , SqlResultQuery(..)
  ) where

import           Data.ApiDataTypes
import           Data.Text         hiding (concat, foldl, map)
import           Data.Aeson
import           TextShow
import           Data.Maybe
import           Data.ByteString.Lazy hiding (append, foldl)


-- | Extracts the action and appends space
extractAction :: Action -> Text
extractAction (Action t) =
  if t == "SELECT"
    then  append t  " "
  else if t == "DELETE"
    then  append t  " FROM "
  else if t == "INSERT"
    then  append t  " INTO "
  else error "Action parameter id wrong"


-- | Extracts table name to Text
extractTableName :: TableName -> Text
extractTableName (TableName t) = t


-- | Extracts table column name to Text
extractColumnName :: ColumnName -> Text
extractColumnName (ColumnName t) = t


-- | Extracts Operator to Text
extractOperator :: Operator -> Text
extractOperator (Operator t) = t


-- | Fetches the Action from SqlQuery
getActionArg :: SqlQuery -> Action
getActionArg q = action q


-- | Gets the main table that Action is performed on
getSelectTableArg :: SqlQuery -> TableName
getSelectTableArg q = selectName q


-- | Retrieves the set list from the SqlQuery
getSetFieldsArg :: SqlQuery -> Maybe [SetField]
getSetFieldsArg q =  set q


-- | Retrieves the join list from the SqlQuery
getJoinTableArg :: SqlQuery -> Maybe [JoinTable]
getJoinTableArg q =  joinTables q


-- | Gets the where condition list
getWhereConditionArg :: SqlQuery -> Maybe [WhereCondition]
getWhereConditionArg q =  whereCondition q


-- | Retrieves the result Format
getFormatArg :: SqlQuery -> Int
getFormatArg q =  getFormat $ Format $ format q


-- | Formats the join table list
formatSetStr :: SetField -> Text
formatSetStr j = (foldl append "" (" set " : [(extractColumnName $ columnName j) , " = " , (formatFieldValue $ setFieldValue j) , " ," ]))


-- | Formats the join table list
formatJoinStr :: JoinTable -> Text
formatJoinStr j = foldl append "" (" join " : [(extractTableName $ tablename j) , " on " , (extractColumnName $ field j) , " " ,(extractOperator $ operator j) , " " , (extractTableName $ withTable j)  , "." , (extractColumnName $ withField j), " " ])


-- | Fetches field value depending on field type
formatFieldValue :: FieldValue -> Text
formatFieldValue a =
  case a of
    IntField x  -> showt x
    TextField x -> showt x
    DateField x -> showt x


-- | Formats WhereCondition to Text
formatWhereConditionStr :: WhereCondition -> Text
formatWhereConditionStr j = foldl append " " (" where " : [ (extractTableName $ whereTableName j), "." , (extractColumnName $ whereField j) , " " ,(extractOperator $ whereOperator j) , " " , (formatFieldValue $ whereFieldValue j)])



-- | Creates final SqlResultQuery type
formatToSqlResultQueryType sql = SqlResultQuery (getActionArg sql) (getSelectTableArg sql) (getSetFieldsArg sql) (getJoinTableArg sql) (getWhereConditionArg sql)



-- | Returns errors for current query
getErrors :: SqlQuery -> Text
getErrors s = t
  where
    t =
      case action s of
        Action "SELECT"
          | isJust (set s) -> " Do not use SET in SELECT query "
          | Data.Text.null (extractTableName $ selectName s)  -> " You are missing the FROM table name in SELECT statement "
          | otherwise -> ""
        Action "DELETE"
          | isJust (joinTables s) -> " Do not use joins in DELETE query "
          | otherwise -> ""
        Action "UPDATE"
          | isJust (joinTables s) -> " Do not use joins in UPDATE query "
          | otherwise -> ""
        Action "INSERT"
          | isJust (joinTables s) -> " Do not use joins in UPDATE query "
          | otherwise -> ""


-- | Returns possible warnings for current query
getWarnings :: SqlQuery -> Text
getWarnings s = t
  where
    t =
      case action s of
        Action "SELECT"
          | isNothing (whereCondition s) ->
            " You are probably missing WHERE statement "
          | otherwise -> ""
        Action "DELETE"
          | isNothing (whereCondition s) ->
            " You are probably missing WHERE statement "
          | otherwise -> ""
        Action "UPDATE"
          | isNothing (whereCondition s) -> " You are missing WHERE statement "
          | otherwise -> ""
        Action "INSERT"
          | isNothing (set s) -> " You are missing the SET statement "
          | otherwise -> ""




-- | Returns json with sql query and errors and warnings if any 
rawSqlStr :: SqlQuery -> ByteString
rawSqlStr s = encode $ SqlResponse {response = alltext, errors = getErrors s , warnings = getWarnings s}
  where
        alltext = foldl append "" [(extractAction $ getAction sql) ,(extractTableName $ getSelectTable sql) , setFields , joins , whereConditions ]
        setFields = case  getSetFields sql of
                      Just x -> Data.Text.init $  foldl append "" $ fmap formatSetStr x
                      Nothing -> ""
        joins = case getJoins sql of
                     Just x -> foldl append "" $ fmap formatJoinStr x
                     Nothing -> ""
        whereConditions = case getWhereCondition sql of
                            Just x -> foldl append "" $ fmap formatWhereConditionStr x
                            Nothing -> ""
        sql = formatToSqlResultQueryType s
