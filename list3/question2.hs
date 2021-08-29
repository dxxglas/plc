module Main where 
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM

reposicao :: Int -> TVar Int -> TVar Int -> TVar Int -> IO ()
reposicao indice cola polo quate
    = do
        returned <- atomically (do
                    if indice == 1 then
                        do
                        v <- readTVar cola
                        writeTVar cola (v + 1000)
                        v <- readTVar cola
                        return v
                    else 
                        if indice == 2 then
                                do
                                v <- readTVar polo
                                writeTVar polo (v + 1000)
                                v <- readTVar polo
                                return v
                        else
                            do
                            v <- readTVar quate
                            writeTVar quate (v + 1000)
                            v <- readTVar quate
                            return v
                    ) 
        threadDelay(1500)
        putStr("O refrigerante " ++ show indice ++ " foi reabastelcido com 1000 ml, e agora possui " ++ show returned ++ " ml" ++ "\n ")
        return()

maquina :: TVar Int -> TVar Int -> TVar Int -> Int -> IO ()
maquina cola polo quate refri
    = do
        returned <- atomically  (do
                    if refri == 1 then
                        do
                        v <- readTVar cola
                        writeTVar cola (v - 300)
                        v <- readTVar cola
                        if v < 1000 then  
                            return 1
                        else
                            return 0
                    else 
                        if refri == 2 then
                            do
                            v <- readTVar polo
                            writeTVar polo (v - 300)
                            v <- readTVar polo
                            if v < 1000 then 
                                return 1
                            else
                                return 0
                        else
                            do
                            v <- readTVar quate
                            writeTVar quate (v - 300)
                            v <- readTVar quate
                            if v < 1000 then 
                                return 1
                            else
                                return 0
                    )

        if returned == 1 then
            reposicao refri cola polo quate
        else
            return()

cliente :: TVar Int -> TVar Int -> TVar Int -> Int -> IO ()
cliente cola polo quate num = do 
    -- o refrigerante que está sendo pedido sempre é o 1 (cola)
    -- não consegui deixar o pedido de refrigerante randomico
    putStr("O cliente " ++ show num ++ " do refrigerante 1 está enchendo seu copo" ++ "\n ")
    maquina cola polo quate 1
    threadDelay (1000)
    cliente cola polo quate (num+1)
       
main :: IO ()
main 
    = do  
        cola <- atomically (newTVar 2000)
        polo <- atomically (newTVar 2000)
        quate <- atomically (newTVar 2000)
        forkIO (cliente cola polo quate 0)
        return ()