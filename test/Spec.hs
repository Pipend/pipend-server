import qualified Test.Hspec as H
import qualified Pipend.Policy as Policy
import Control.Monad (ap)

makeCan can p r l = H.it (show r ++ " should have " ++ show l ++ " permission") $ can r l p
makeCant can p r l = H.it (show r ++ " should not have " ++ show l ++ " permission") $ not (can r l p)

ownerHasAllAccess can = do
  Policy.Owner `can` Policy.View
  Policy.Owner `can` Policy.Edit

publicHasNoAccess cant = do
  Policy.Public `cant` Policy.View
  Policy.Public `cant` Policy.Edit

defaultExecutionAccess can cant = do
  Policy.Owner `can` Policy.Execute
  Policy.Contributor `can` Policy.Execute
  Policy.Public `cant` Policy.Execute

testDefaultQueryAccessPolicy = H.describe "Default Query Access Policy" $ do
  ownerHasAllAccess can
  Policy.Contributor `can` Policy.View
  Policy.Contributor `can` Policy.Edit
  publicHasNoAccess cant
  where
    can = makeCan Policy.canAccess Policy.defaultQueryAccessPolicy
    cant = makeCant Policy.canAccess Policy.defaultQueryAccessPolicy

testDefaultQueryExecutionPolicy = H.describe "Default Query Execution Policy" $
  defaultExecutionAccess can cant where
    can = makeCan Policy.canExecute Policy.defaultQueryExecutionPolicy
    cant = makeCant Policy.canExecute Policy.defaultQueryExecutionPolicy

testDefaultDataSourceAccessPolicy = H.describe "Default DataSource Access Policy" $ do
  ownerHasAllAccess can
  Policy.Contributor `can` Policy.View
  Policy.Contributor `cant` Policy.Edit
  publicHasNoAccess cant
  where
    can = makeCan Policy.canAccess Policy.defaultDataSourceAccessPolicy
    cant = makeCant Policy.canAccess Policy.defaultDataSourceAccessPolicy

testDefaultDataSourceExecutionPolicy = H.describe "Default DataSource Execution Policy" $
  defaultExecutionAccess can cant where
    can = makeCan Policy.canExecute Policy.defaultDataSourceExecutionPolicy
    cant = makeCant Policy.canExecute Policy.defaultDataSourceExecutionPolicy

main :: IO ()
main = H.hspec $ H.parallel $ do
  testDefaultQueryAccessPolicy
  testDefaultQueryExecutionPolicy
  testDefaultDataSourceAccessPolicy
  testDefaultDataSourceExecutionPolicy
