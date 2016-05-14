# Splicing via Parameter

A couple of weeks ago I was attempting to add functionatlity to this site to permit the retrieval of articles individually. I found it challenging to locate any working examples where data from an HTTP parameter is used to form a query to pull down results from a database to be used in a compiled splice. Here's what I eventually came up with:

~~~~ {.haskell}
articleSpliceById :: (HasPostgres n, MonadSnap n) => C.Splice n
articleSpliceById = do
  promise <- C.newEmptyPromise
  outputChildren <- C.manyWithSplices C.runChildren articleSplices (C.getPromise promise)
  return $ C.yieldRuntime $ do
    id <- lift $ getParam "id"
    articles <- lift $ query "SELECT * FROM article WHERE id = ?" (Only id)
    C.putPromise promise articles >> C.codeGen outputChildren
    
articleSplice :: (HasPostgres n, MonadSnap n) => Splices (C.Splice n)
articleSplice = "article" ## articleSpliceById
~~~~

It's been suggested to me by wiser folks that `deferMap` or `mayDeferMap` might be a better solution that would save me from having to deal with `promise` directly. I haven't dug in to that yet, but plan to read up on those options.

Regardless, given the dearth of specific examples out there, I thought it would be helpful to put my own out there. Happy trails!
