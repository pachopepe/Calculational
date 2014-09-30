{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Calculational

-- | Some sample expressions for the library

-- | Gender type
data Gender = Male
            | Female
            deriving (Show,Eq,Ord,Read)

-- | Person identifier type
type IdPerson = Integer

-- | Employee identifier type
type IdEmployee = Integer

-- | Person datatype
data Person = Person { idPerson :: IdPerson,
                       name :: String,
                       age :: Integer,
                       gender :: Gender }
              deriving (Show,Eq,Ord)

-- | Employee datatype
data Employee = Employee { idEmployee :: IdEmployee,
                           idPersonE :: IdPerson,
                           salary :: Integer }
              deriving (Show,Eq,Ord)


-- List of persons
persons :: [Person]
persons = [ Person 1 "John"   16 Male 
          , Person 2 "Mary"   18 Female 
          , Person 3 "Martha" 22 Female 
          , Person 4 "Ruby"   22 Female 
          , Person 5 "Peter"  28 Male
          , Person 6 "Paul"   26 Male
          , Person 7 "Jaques" 26 Male
          ]

-- List of employees
employees :: [Employee]
employees = [ Employee 100 1 1500 
            , Employee 200 2 1200 
            , Employee 300 3 1200 
            , Employee 400 4  900
            , Employee 500 5 1500
            , Employee 600 6 2000
            ]

-- | Payroll
payroll :: Integer
payroll =  [calc| (+ e <- employees : : salary e ) |]

-- | Average Salary
averageSalary :: Double
averageSalary = average [calc| (avg e <- employees : : salary e) |]

-- | Set of person names with salary higher than 'mn'
salaryHigher :: Integer -> Set String
salaryHigher mn = [calc| { e <- employees, p <- persons : 
                           idPerson p == idEmployee e /\ salary e > mn : 
                           name p } |]

-- | List of persons with salary higher than mn
salaryHigher' :: Integer -> [Person]
salaryHigher' mn = [calc| [ e <- employees, p <- persons :
                            salary e >= mn /\ idEmployee e == idPerson p :
                            p ] |] 

-- | List of (person,employee) pairs with age between @mn@ and @mx@
employeesBetweenAge :: Integer -> Integer -> Set (Person,Employee)
employeesBetweenAge mn mx =
  [calc| { p <- persons, e <- employees : 
           mn <= age p <= mx /\ idPerson p == idPersonE e :
           (p,e) } |]

-- | Average salary of persons with age between mn and mx  
averageSalary' :: Integer -> Integer -> Double
averageSalary' mn mx = average [calc| (avg (_,e) <- es : : salary e ) |]
            where es = employeesBetweenAge mn mx

-- | salary frequency of employees 
salaryFrequence :: MultiSet Integer
salaryFrequence = [calc| {| e <- employees : : salary e |} |]

-- | age frequency of persons
ageFrequency :: MultiSet Integer
ageFrequency = [calc| {| p <- persons : : age p |} |]
