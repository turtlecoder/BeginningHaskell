module Chapter06.Lens where

data Client i = GovOrg i String
              | Company i String Person String
              | Indivdual i Person

data Person = Person String String

firstName = undefined
