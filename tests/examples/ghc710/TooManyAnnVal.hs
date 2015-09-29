

-- UUAGC 0.9.38.6 (./src/SistemaL.ag)
module SistemaL where
{-# LINE 3 "./src/SistemaL.ag" #-}

import Data.List
{-# LINE 9 "./src/SistemaL.hs" #-}

{-# LINE 69 "./src/SistemaL.ag" #-}

addIdentProds prods alfa
    = let prods' = map (\(Simbolo e,_) -> e) prods
          resto  = alfa \\ prods'
          iprods = map (\e -> (Simbolo e, [Simbolo e])) resto
      in prods ++ iprods

myElem _ [] = False
myElem e1 ((Simbolo e2,_):xs) = if e1 == e2
                                then True
                                else myElem e1 xs
{-# LINE 23 "./src/SistemaL.hs" #-}

{-# LINE 125 "./src/SistemaL.ag" #-}

ejemplo1 = SistemaL "Koch" alfaK initK prodK
alfaK = [Simbolo "F", Simbolo "f", Simbolo "+", Simbolo "-"]
initK = [Simbolo "F", Simbolo "a"]
prodK = [ (Simbolo "F", [Simbolo "F", Simbolo "g"])
        , (Simbolo "F", [])
        ]

ejemplo2 = SistemaL "Koch" alfaK2 initK2 prodK2
alfaK2 = [Simbolo "F", Simbolo "f", Simbolo "+", Simbolo "-"]
initK2 = [Simbolo "F", Simbolo "f"]
prodK2 = [ (Simbolo "F", [Simbolo "F", Simbolo "+"])
        , (Simbolo "f", [])
        ]

getNombre (SistemaL nm _ _ _) = nm

testSistemaL :: SistemaL -> Either [String] SistemaL
testSistemaL = sem_SistemaL
{-# LINE 45 "./src/SistemaL.hs" #-}
-- Alfabeto ----------------------------------------------------
type Alfabeto  = [Simbolo ]
-- cata
sem_Alfabeto :: Alfabeto  ->
                T_Alfabeto
sem_Alfabeto list  =
    (Prelude.foldr sem_Alfabeto_Cons sem_Alfabeto_Nil (Prelude.map sem_Simbolo list) )
-- semantic domain
type T_Alfabeto  = ([String]) ->
                   ( ([String]),([String]),Alfabeto )
sem_Alfabeto_Cons :: T_Simbolo  ->
                     T_Alfabeto  ->
                     T_Alfabeto
sem_Alfabeto_Cons hd_ tl_  =
    (\ _lhsIalf ->
         (let _tlOalf :: ([String])
              _lhsOalf :: ([String])
              _lhsOerrores :: ([String])
              _lhsOself :: Alfabeto
              _hdIself :: Simbolo
              _hdIsimb :: String
              _tlIalf :: ([String])
              _tlIerrores :: ([String])
              _tlIself :: Alfabeto
              _verificar =
                  ({-# LINE 31 "./src/SistemaL.ag" #-}
                   elem _hdIsimb _lhsIalf
                   {-# LINE 73 "./src/SistemaL.hs" #-}
                   )
              _tlOalf =
                  ({-# LINE 32 "./src/SistemaL.ag" #-}
                   if _verificar
                   then _lhsIalf
                   else _hdIsimb : _lhsIalf
                   {-# LINE 80 "./src/SistemaL.hs" #-}
                   )
              _lhsOalf =
                  ({-# LINE 35 "./src/SistemaL.ag" #-}
                   _tlIalf
                   {-# LINE 85 "./src/SistemaL.hs" #-}
                   )
              _lhsOerrores =
                  ({-# LINE 93 "./src/SistemaL.ag" #-}
                   if _verificar
                   then ("El simbolo: '" ++ _hdIsimb ++ "' esta repetido mas de una ves en el alfabeto.") : _tlIerrores
                   else _tlIerrores
                   {-# LINE 92 "./src/SistemaL.hs" #-}
                   )
              _self =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   (:) _hdIself _tlIself
                   {-# LINE 97 "./src/SistemaL.hs" #-}
                   )
              _lhsOself =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   _self
                   {-# LINE 102 "./src/SistemaL.hs" #-}
                   )
              ( _hdIself,_hdIsimb) =
                  hd_
              ( _tlIalf,_tlIerrores,_tlIself) =
                  tl_ _tlOalf
          in  ( _lhsOalf,_lhsOerrores,_lhsOself)))
sem_Alfabeto_Nil :: T_Alfabeto
sem_Alfabeto_Nil  =
    (\ _lhsIalf ->
         (let _lhsOalf :: ([String])
              _lhsOerrores :: ([String])
              _lhsOself :: Alfabeto
              _lhsOalf =
                  ({-# LINE 36 "./src/SistemaL.ag" #-}
                   _lhsIalf
                   {-# LINE 118 "./src/SistemaL.hs" #-}
                   )
              _lhsOerrores =
                  ({-# LINE 96 "./src/SistemaL.ag" #-}
                   []
                   {-# LINE 123 "./src/SistemaL.hs" #-}
                   )
              _self =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   []
                   {-# LINE 128 "./src/SistemaL.hs" #-}
                   )
              _lhsOself =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   _self
                   {-# LINE 133 "./src/SistemaL.hs" #-}
                   )
          in  ( _lhsOalf,_lhsOerrores,_lhsOself)))
-- Inicio ------------------------------------------------------
type Inicio  = [Simbolo ]
-- cata
sem_Inicio :: Inicio  ->
              T_Inicio
sem_Inicio list  =
    (Prelude.foldr sem_Inicio_Cons sem_Inicio_Nil (Prelude.map sem_Simbolo list) )
-- semantic domain
type T_Inicio  = ([String]) ->
                 ( ([String]),Inicio )
sem_Inicio_Cons :: T_Simbolo  ->
                   T_Inicio  ->
                   T_Inicio
sem_Inicio_Cons hd_ tl_  =
    (\ _lhsIalfabeto ->
         (let _lhsOerrores :: ([String])
              _lhsOself :: Inicio
              _tlOalfabeto :: ([String])
              _hdIself :: Simbolo
              _hdIsimb :: String
              _tlIerrores :: ([String])
              _tlIself :: Inicio
              _lhsOerrores =
                  ({-# LINE 99 "./src/SistemaL.ag" #-}
                   if elem _hdIsimb _lhsIalfabeto
                   then _tlIerrores
                   else ("El simbolo de inicio: '" ++ _hdIsimb ++ "' no se encuentra en el alfabeto.") : _tlIerrores
                   {-# LINE 163 "./src/SistemaL.hs" #-}
                   )
              _self =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   (:) _hdIself _tlIself
                   {-# LINE 168 "./src/SistemaL.hs" #-}
                   )
              _lhsOself =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   _self
                   {-# LINE 173 "./src/SistemaL.hs" #-}
                   )
              _tlOalfabeto =
                  ({-# LINE 39 "./src/SistemaL.ag" #-}
                   _lhsIalfabeto
                   {-# LINE 178 "./src/SistemaL.hs" #-}
                   )
              ( _hdIself,_hdIsimb) =
                  hd_
              ( _tlIerrores,_tlIself) =
                  tl_ _tlOalfabeto
          in  ( _lhsOerrores,_lhsOself)))
sem_Inicio_Nil :: T_Inicio
sem_Inicio_Nil  =
    (\ _lhsIalfabeto ->
         (let _lhsOerrores :: ([String])
              _lhsOself :: Inicio
              _lhsOerrores =
                  ({-# LINE 102 "./src/SistemaL.ag" #-}
                   []
                   {-# LINE 193 "./src/SistemaL.hs" #-}
                   )
              _self =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   []
                   {-# LINE 198 "./src/SistemaL.hs" #-}
                   )
              _lhsOself =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   _self
                   {-# LINE 203 "./src/SistemaL.hs" #-}
                   )
          in  ( _lhsOerrores,_lhsOself)))
-- Produccion --------------------------------------------------
type Produccion  = ( Simbolo ,Succesor )
-- cata
sem_Produccion :: Produccion  ->
                  T_Produccion
sem_Produccion ( x1,x2)  =
    (sem_Produccion_Tuple (sem_Simbolo x1 ) (sem_Succesor x2 ) )
-- semantic domain
type T_Produccion  = ([String]) ->
                     ( ([String]),Produccion ,String)
sem_Produccion_Tuple :: T_Simbolo  ->
                        T_Succesor  ->
                        T_Produccion
sem_Produccion_Tuple x1_ x2_  =
    (\ _lhsIalfabeto ->
         (let _lhsOerrores :: ([String])
              _lhsOself :: Produccion
              _lhsOsimb :: String
              _x2Oalfabeto :: ([String])
              _x1Iself :: Simbolo
              _x1Isimb :: String
              _x2Ierrores :: ([String])
              _x2Iself :: Succesor
              _lhsOerrores =
                  ({-# LINE 114 "./src/SistemaL.ag" #-}
                   if elem _x1Isimb _lhsIalfabeto
                   then _x2Ierrores
                   else ("El simbolo de la produccion (izq): '" ++ _x1Isimb ++ "' no se encuentra en el alfabeto.") : _x2Ierrores
                   {-# LINE 234 "./src/SistemaL.hs" #-}
                   )
              _self =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   (_x1Iself,_x2Iself)
                   {-# LINE 239 "./src/SistemaL.hs" #-}
                   )
              _lhsOself =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   _self
                   {-# LINE 244 "./src/SistemaL.hs" #-}
                   )
              _lhsOsimb =
                  ({-# LINE 45 "./src/SistemaL.ag" #-}
                   _x1Isimb
                   {-# LINE 249 "./src/SistemaL.hs" #-}
                   )
              _x2Oalfabeto =
                  ({-# LINE 39 "./src/SistemaL.ag" #-}
                   _lhsIalfabeto
                   {-# LINE 254 "./src/SistemaL.hs" #-}
                   )
              ( _x1Iself,_x1Isimb) =
                  x1_
              ( _x2Ierrores,_x2Iself) =
                  x2_ _x2Oalfabeto
          in  ( _lhsOerrores,_lhsOself,_lhsOsimb)))
-- Producciones ------------------------------------------------
type Producciones  = [Produccion ]
-- cata
sem_Producciones :: Producciones  ->
                    T_Producciones
sem_Producciones list  =
    (Prelude.foldr sem_Producciones_Cons sem_Producciones_Nil (Prelude.map sem_Produccion list) )
-- semantic domain
type T_Producciones  = ([String]) ->
                       Producciones ->
                       ( ([String]),Producciones)
sem_Producciones_Cons :: T_Produccion  ->
                         T_Producciones  ->
                         T_Producciones
sem_Producciones_Cons hd_ tl_  =
    (\ _lhsIalfabeto
       _lhsIprods ->
         (let _tlOprods :: Producciones
              _lhsOprods :: Producciones
              _lhsOerrores :: ([String])
              _hdOalfabeto :: ([String])
              _tlOalfabeto :: ([String])
              _hdIerrores :: ([String])
              _hdIself :: Produccion
              _hdIsimb :: String
              _tlIerrores :: ([String])
              _tlIprods :: Producciones
              _verificar =
                  ({-# LINE 60 "./src/SistemaL.ag" #-}
                   myElem _hdIsimb _lhsIprods
                   {-# LINE 291 "./src/SistemaL.hs" #-}
                   )
              _tlOprods =
                  ({-# LINE 61 "./src/SistemaL.ag" #-}
                   if _verificar
                   then _lhsIprods
                   else _hdIself : _lhsIprods
                   {-# LINE 298 "./src/SistemaL.hs" #-}
                   )
              _lhsOprods =
                  ({-# LINE 64 "./src/SistemaL.ag" #-}
                   _tlIprods
                   {-# LINE 303 "./src/SistemaL.hs" #-}
                   )
              _lhsOerrores =
                  ({-# LINE 105 "./src/SistemaL.ag" #-}
                   if _verificar
                   then let error = "La produccion con el simb. izq.:'"
                                 ++ _hdIsimb
                                 ++ "' esta repetida mas de una ves en la lista de producciones."
                        in (error : _hdIerrores) ++ _tlIerrores
                   else _hdIerrores ++ _tlIerrores
                   {-# LINE 313 "./src/SistemaL.hs" #-}
                   )
              _hdOalfabeto =
                  ({-# LINE 39 "./src/SistemaL.ag" #-}
                   _lhsIalfabeto
                   {-# LINE 318 "./src/SistemaL.hs" #-}
                   )
              _tlOalfabeto =
                  ({-# LINE 39 "./src/SistemaL.ag" #-}
                   _lhsIalfabeto
                   {-# LINE 323 "./src/SistemaL.hs" #-}
                   )
              ( _hdIerrores,_hdIself,_hdIsimb) =
                  hd_ _hdOalfabeto
              ( _tlIerrores,_tlIprods) =
                  tl_ _tlOalfabeto _tlOprods
          in  ( _lhsOerrores,_lhsOprods)))
sem_Producciones_Nil :: T_Producciones
sem_Producciones_Nil  =
    (\ _lhsIalfabeto
       _lhsIprods ->
         (let _lhsOerrores :: ([String])
              _lhsOprods :: Producciones
              _lhsOerrores =
                  ({-# LINE 111 "./src/SistemaL.ag" #-}
                   []
                   {-# LINE 339 "./src/SistemaL.hs" #-}
                   )
              _lhsOprods =
                  ({-# LINE 58 "./src/SistemaL.ag" #-}
                   _lhsIprods
                   {-# LINE 344 "./src/SistemaL.hs" #-}
                   )
          in  ( _lhsOerrores,_lhsOprods)))
-- Simbolo -----------------------------------------------------
data Simbolo  = Simbolo (String)
              deriving ( Eq,Show)
-- cata
sem_Simbolo :: Simbolo  ->
               T_Simbolo
sem_Simbolo (Simbolo _string )  =
    (sem_Simbolo_Simbolo _string )
-- semantic domain
type T_Simbolo  = ( Simbolo ,String)
sem_Simbolo_Simbolo :: String ->
                       T_Simbolo
sem_Simbolo_Simbolo string_  =
    (let _lhsOsimb :: String
         _lhsOself :: Simbolo
         _lhsOsimb =
             ({-# LINE 47 "./src/SistemaL.ag" #-}
              string_
              {-# LINE 365 "./src/SistemaL.hs" #-}
              )
         _self =
             ({-# LINE 57 "./src/SistemaL.ag" #-}
              Simbolo string_
              {-# LINE 370 "./src/SistemaL.hs" #-}
              )
         _lhsOself =
             ({-# LINE 57 "./src/SistemaL.ag" #-}
              _self
              {-# LINE 375 "./src/SistemaL.hs" #-}
              )
     in  ( _lhsOself,_lhsOsimb))
-- SistemaL ----------------------------------------------------
data SistemaL  = SistemaL (String) (Alfabeto ) (Inicio ) (Producciones )
               deriving ( Show)
-- cata
sem_SistemaL :: SistemaL  ->
                T_SistemaL
sem_SistemaL (SistemaL _nombre _alfabeto _inicio _producciones )  =
    (sem_SistemaL_SistemaL _nombre (sem_Alfabeto _alfabeto ) (sem_Inicio _inicio ) (sem_Producciones _producciones ) )
-- semantic domain
type T_SistemaL  = ( (Either [String] SistemaL))
sem_SistemaL_SistemaL :: String ->
                         T_Alfabeto  ->
                         T_Inicio  ->
                         T_Producciones  ->
                         T_SistemaL
sem_SistemaL_SistemaL nombre_ alfabeto_ inicio_ producciones_  =
    (let _alfabetoOalf :: ([String])
         _inicioOalfabeto :: ([String])
         _produccionesOalfabeto :: ([String])
         _lhsOresultado :: (Either [String] SistemaL)
         _produccionesOprods :: Producciones
         _alfabetoIalf :: ([String])
         _alfabetoIerrores :: ([String])
         _alfabetoIself :: Alfabeto
         _inicioIerrores :: ([String])
         _inicioIself :: Inicio
         _produccionesIerrores :: ([String])
         _produccionesIprods :: Producciones
         _alfabetoOalf =
             ({-# LINE 28 "./src/SistemaL.ag" #-}
              []
              {-# LINE 409 "./src/SistemaL.hs" #-}
              )
         _inicioOalfabeto =
             ({-# LINE 41 "./src/SistemaL.ag" #-}
              _alfabetoIalf
              {-# LINE 414 "./src/SistemaL.hs" #-}
              )
         _produccionesOalfabeto =
             ({-# LINE 42 "./src/SistemaL.ag" #-}
              _alfabetoIalf
              {-# LINE 419 "./src/SistemaL.hs" #-}
              )
         _lhsOresultado =
             ({-# LINE 52 "./src/SistemaL.ag" #-}
              if null _errores
              then let producciones = addIdentProds _produccionesIprods _alfabetoIalf
                   in Right (SistemaL nombre_ _alfabetoIself _inicioIself producciones)
              else Left _errores
              {-# LINE 427 "./src/SistemaL.hs" #-}
              )
         _produccionesOprods =
             ({-# LINE 67 "./src/SistemaL.ag" #-}
              []
              {-# LINE 432 "./src/SistemaL.hs" #-}
              )
         _errores =
             ({-# LINE 85 "./src/SistemaL.ag" #-}
              let inicioErr = if null _inicioIself
                              then "La lista de simbolos de inicio no puede ser vacia" : _inicioIerrores
                              else _inicioIerrores
                  errores   = map (\err -> nombre_ ++ ": " ++ err) (_alfabetoIerrores ++ inicioErr ++ _produccionesIerrores)
              in errores
              {-# LINE 441 "./src/SistemaL.hs" #-}
              )
         ( _alfabetoIalf,_alfabetoIerrores,_alfabetoIself) =
             alfabeto_ _alfabetoOalf
         ( _inicioIerrores,_inicioIself) =
             inicio_ _inicioOalfabeto
         ( _produccionesIerrores,_produccionesIprods) =
             producciones_ _produccionesOalfabeto _produccionesOprods
     in  ( _lhsOresultado))
-- Succesor ----------------------------------------------------
type Succesor  = [Simbolo ]
-- cata
sem_Succesor :: Succesor  ->
                T_Succesor
sem_Succesor list  =
    (Prelude.foldr sem_Succesor_Cons sem_Succesor_Nil (Prelude.map sem_Simbolo list) )
-- semantic domain
type T_Succesor  = ([String]) ->
                   ( ([String]),Succesor )
sem_Succesor_Cons :: T_Simbolo  ->
                     T_Succesor  ->
                     T_Succesor
sem_Succesor_Cons hd_ tl_  =
    (\ _lhsIalfabeto ->
         (let _lhsOerrores :: ([String])
              _lhsOself :: Succesor
              _tlOalfabeto :: ([String])
              _hdIself :: Simbolo
              _hdIsimb :: String
              _tlIerrores :: ([String])
              _tlIself :: Succesor
              _lhsOerrores =
                  ({-# LINE 119 "./src/SistemaL.ag" #-}
                   if elem _hdIsimb _lhsIalfabeto
                   then _tlIerrores
                   else ("El simbolo de la produccion (der): '" ++ _hdIsimb ++ "' no se encuentra en el alfabeto.") : _tlIerrores
                   {-# LINE 477 "./src/SistemaL.hs" #-}
                   )
              _self =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   (:) _hdIself _tlIself
                   {-# LINE 482 "./src/SistemaL.hs" #-}
                   )
              _lhsOself =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   _self
                   {-# LINE 487 "./src/SistemaL.hs" #-}
                   )
              _tlOalfabeto =
                  ({-# LINE 39 "./src/SistemaL.ag" #-}
                   _lhsIalfabeto
                   {-# LINE 492 "./src/SistemaL.hs" #-}
                   )
              ( _hdIself,_hdIsimb) =
                  hd_
              ( _tlIerrores,_tlIself) =
                  tl_ _tlOalfabeto
          in  ( _lhsOerrores,_lhsOself)))
sem_Succesor_Nil :: T_Succesor
sem_Succesor_Nil  =
    (\ _lhsIalfabeto ->
         (let _lhsOerrores :: ([String])
              _lhsOself :: Succesor
              _lhsOerrores =
                  ({-# LINE 122 "./src/SistemaL.ag" #-}
                   []
                   {-# LINE 507 "./src/SistemaL.hs" #-}
                   )
              _self =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   []
                   {-# LINE 512 "./src/SistemaL.hs" #-}
                   )
              _lhsOself =
                  ({-# LINE 57 "./src/SistemaL.ag" #-}
                   _self
                   {-# LINE 517 "./src/SistemaL.hs" #-}
                   )
          in  ( _lhsOerrores,_lhsOself)))
