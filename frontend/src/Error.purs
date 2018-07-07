module Error where


data EditorError
  = EditorSaveFailed
  | EditorSaveSuccess


data SiteError
  = SiteErrorEditor EditorError


printSiteError :: SiteError -> String
printSiteError e = case e of
  SiteErrorEditor cust -> case cust of
    EditorSaveFailed -> "Internal error - couldn't save editor details"
    EditorSaveSuccess -> "Editor details saved."
