implementation module iTasks.Gin.Monad


(>>>|) infixr 5 :: (m a) (m b) -> m b | Monad m
(>>>|) a b = a >>> \_ = b
