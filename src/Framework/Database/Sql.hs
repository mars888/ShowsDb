module Framework.Database.Sql (
  module Framework.Database.Sql.Types
, module Framework.Database.Sql.Convertible
, module Framework.Database.Sql.Operations
, module Framework.Database.Sql.Generation
) where

import Framework.Database.Sql.Types
import Framework.Database.Sql.Convertible
import Framework.Database.Sql.Operations
import Framework.Database.Sql.Generation


stest = select ["a", "b"] `from` "Foos" `whereIs` ("a" #=# (10::Int) `andWhere` "b" #=# "foo")
utest = update "foos" ["a" #># "val1", "b" #># "val2"] `whereIs` "id" #=# (10::Int)
dtest = deleteFrom "bars" `whereIs` "id" #=# (10::Int)
itest = insertInto "quix" `insertValues` ["a" #># "bar", "b" #># (30::Int)]

stest' = (toQuery stest, toParams stest)
utest' = (toQuery utest, toParams utest)
dtest' = (toQuery dtest, toParams dtest)
itest' = (toQuery itest, toParams itest)


