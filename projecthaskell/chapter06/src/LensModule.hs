{-# LANGUAGE LambdaCase #-}
import Control.Lens

data Client i = GovOrg i String
              | Company i String Person String
              | Individual i Person

data Person = Person String String

firstName :: Simple Lens Person String
firstName = lens (\(Person f _) -> f ) (\(Person _ l) newF -> Person newF l)

lastName :: Simple Lens Person String
lastName = lens (\(Person _ l) -> l) (\(Person f _) newL -> Person f newL)

identifier :: Lens (Client i) (Client j) i j
identifier = lens (\case (GovOrg i _ )  -> i
                         (Company i _ _ _ ) -> i
                         (Individual i _) -> i)
             (\client newID -> case client of
                                 GovOrg _ n -> GovOrg newID n
                                 Company _ n p r -> Company newID n p r
                                 Individual _ p -> Individual newID p)


fullName :: Simple Lens Person String
fullName = lens (\(Person f l) -> f ++ " " ++ l)
                (\ _ newFullName -> case words newFullName of
                                      f:l:_ -> Person f l
                                      _ -> error "Incorrect name")
