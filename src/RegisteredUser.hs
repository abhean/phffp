module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = print "Unregistered User"
printUser (RegisteredUser (Username userName) (AccountNumber accountNumber)) =
    putStrLn ("Registered User, name = " ++ userName ++
              ", account: " ++ show accountNumber)
