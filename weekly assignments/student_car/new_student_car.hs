{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
import Debug.Trace ( trace )
import Data.Function ( (&) )

data Car = Car  { pass :: Int
                , maxPass :: Int
                , gas :: Int
                , maxGas :: Int
                , km :: Int
                } deriving (Eq, Show, Read)


data Op = Op { name :: String
             , result :: Bool
             } deriving (Eq, Show, Read)

data Info = Info { car :: Car
                 , op  :: Op
                 } deriving (Eq, Show, Read)


toString (Info (Car pass maxPass gas maxGas km) (Op name result)) =
                "Car pass: " ++ show pass ++ "/" ++ show maxPass
                 ++ " gas: " ++ show gas  ++ "/" ++ show maxGas
                 ++  " km: " ++ show km
                 ++ " Operation: " ++ name ++ " Result: " ++ show result

resume :: Info -> Info
resume info = trace (toString info) info

-- cria um carro passando maxPass e maxGas - retorna sempre true.
createCar :: Int -> Int -> Info
createCar mPass mGas = myCar
    where
        myCar = Info { car = newCar, op = operation }
        newCar = Car { pass = 0, maxPass = mPass, gas = 0, maxGas = mGas, km = 0 }
        operation = Op {name = "drive", result = True}

-- enche o tanque passando a qtd de gas. Retorna falso apenas se o tanque já estiver completamente cheio.
fuel :: Int -> Info -> Info
fuel qtdGas myCar = newInfo
    where
        myCarGas = gas $ car myCar
        myCarMaxGas = maxGas $ car myCar
        myCarPass = pass $ car myCar
        myCarMaxPass = maxPass $ car myCar
        myCarKm = km $ car myCar
        sumGas = qtdGas + myCarGas
        isFull = sumGas > myCarMaxGas
        oldCar = Car { pass = myCarPass, maxPass = myCarMaxPass, gas = myCarGas, maxGas = myCarMaxGas, km = myCarKm }
        newCar = Car { pass = myCarPass, maxPass = myCarMaxPass, gas = sumGas, maxGas = myCarMaxGas, km = myCarKm }
        failOp = Op { name = "fuel", result = False }
        newOp = Op { name = "fuel", result = True }
        newInfo = if isFull then Info { car = oldCar, op = failOp } else Info { car = newCar, op = newOp }

-- Faz entrar uma pessoa no carro. Retorna false se já estiver lotado.
embark :: Info -> Info 
embark myCar = newInfo
    where
        myCarGas = gas $ car myCar
        myCarMaxGas = maxGas $ car myCar
        myCarPass = pass $ car myCar
        myCarMaxPass = maxPass $ car myCar
        myCarKm = km $ car myCar
        isCarFull = myCarPass + 1 > myCarMaxPass
        oldCar = Car { pass = myCarPass, maxPass = myCarMaxPass, gas = myCarGas, maxGas = myCarMaxGas, km = myCarKm }
        newCar = Car { pass = myCarPass+1, maxPass = myCarMaxPass, gas = myCarGas, maxGas = myCarMaxGas, km = myCarKm }
        failOp = Op { name = "embark", result = False }
        newOp = Op { name = "embark", result = True }
        newInfo = if isCarFull then Info { car = oldCar, op = failOp } else Info { car = newCar, op = newOp }

-- Retira uma pessoa do carro, retorna false se não tiver ninguém no carro
disembark :: Info -> Info
disembark myCar = newInfo
    where
        myCarGas = gas $ car myCar
        myCarMaxGas = maxGas $ car myCar
        myCarPass = pass $ car myCar
        myCarMaxPass = maxPass $ car myCar
        myCarKm = km $ car myCar
        isCarEmpty = myCarPass -1 < 0
        oldCar = Car { pass = myCarPass, maxPass = myCarMaxPass, gas = myCarGas, maxGas = myCarMaxGas, km = myCarKm }
        newCar = Car { pass = myCarPass-1, maxPass = myCarMaxPass, gas = myCarGas, maxGas = myCarMaxGas, km = myCarKm }
        failOp = Op { name = "disembark", result = False }
        newOp = Op { name = "disembark", result = True }
        newInfo = if isCarEmpty then Info { car = oldCar, op = failOp } else Info { car = newCar, op = newOp }

-- dirige diminuindo a gasolina e aumentando km. 
-- Só é possível dirigir se houver alguém no carro e houver alguma gasolina.
-- Aumenta a km da gasolina gasta.
-- retorna false se não há ninguém no carro ou se não tinha gasolina para completar a viagem.
drive :: Int -> Info -> Info
drive qtd myCar = newInfo
    where
        myCarGas = gas $ car myCar
        myCarMaxGas = maxGas $ car myCar
        myCarPass = pass $ car myCar
        myCarMaxPass = maxPass $ car myCar
        myCarKm = km $ car myCar
        isCarEmpty = myCarPass == 0
        isFuel = myCarGas < qtd
        oldCar = Car { pass = myCarPass, maxPass = myCarMaxPass, gas = 0, maxGas = myCarMaxGas, km = myCarKm + myCarGas }
        newCar = Car { pass = myCarPass, maxPass = myCarMaxPass, gas = myCarGas-qtd, maxGas = myCarMaxGas, km = qtd }
        failOp = Op { name = "drive", result = False }
        newOp = Op { name = "drive", result = True }
        newInfo = if isCarEmpty || isFuel then Info { car = oldCar, op = failOp } else Info { car = newCar, op = newOp }

-- main = print $ resume . embark . resume. embark . resume $ createCar 2 50
main = do 
    let res = createCar 2 50 
            & resume & embark
            & resume & disembark
            & resume & disembark
            & resume & drive 10
            & resume & embark
            & resume & embark
            & resume & embark
            & resume & drive 10
            & resume & fuel 30
            & resume & fuel 20
            & resume & fuel 30
            & resume & drive 30
            & resume & drive 30
            & resume
    print res 

{-
Car pass: 0/2 gas: 0/50 km: 0 Operation: create Result: True
Car pass: 1/2 gas: 0/50 km: 0 Operation: embark Result: True
Car pass: 0/2 gas: 0/50 km: 0 Operation: disembark Result: True
Car pass: 0/2 gas: 0/50 km: 0 Operation: disembark Result: False
Car pass: 0/2 gas: 0/50 km: 0 Operation: drive Result: False
Car pass: 1/2 gas: 0/50 km: 0 Operation: embark Result: True
Car pass: 2/2 gas: 0/50 km: 0 Operation: embark Result: True
Car pass: 2/2 gas: 0/50 km: 0 Operation: embark Result: False
Car pass: 2/2 gas: 0/50 km: 0 Operation: drive Result: False
Car pass: 2/2 gas: 30/50 km: 0 Operation: fuel Result: True
Car pass: 2/2 gas: 50/50 km: 0 Operation: fuel Result: True
Car pass: 2/2 gas: 50/50 km: 0 Operation: fuel Result: False
Car pass: 2/2 gas: 20/50 km: 30 Operation: drive Result: True
Car pass: 2/2 gas: 0/50 km: 50 Operation: drive Result: False
Info {car = Car {pass = 2, maxPass = 2, gas = 0, maxGas = 50, km = 50}, op = Op {name = "drive", result = False}}
-}