{-# OPTIONS -cpp #-}
module CommonUI where

import GUIEvents
import SafetyNet
import State
import StateUtil
import Network
import NetworkView
import DocumentFile
import Document
import INRule
import INRules
import Common
import CommonIO
import qualified PersistentDocument as PD
import qualified PDDefaults as PD
import Palette
import Shape
import Ports
import Math
import InfoKind
import Constants
import Text.XML.HaXml.XmlContent (XmlContent)
import Text.Parse as Parse
import INChecks
import SpecialSymbols

import Graphics.UI.WX hiding (Child, upKey, downKey, swap)
import Graphics.UI.WXCore hiding (Document, Palette)

import Data.Maybe
import Data.List
import qualified Data.Map as Map


noImage = -1 :: Int

-- | Prints a document in a none XMl format
printy :: (InfoKind n g, InfoKind e g, Show g) => Document.Document g n e -> IO ()
printy doc =
     do let network = getNetwork doc
            rules   = getRules doc
        mostraNodos network
        putStrLn "+++++++++++++++++++++++++++++++"
        mapM_ f rules
     where mostraNodos network = print $ map Network.getName $ getNodes network
           f rule = do putStrLn $ INRule.getName rule
                       putStrLn "lhs"
                       mostraNodos $ getLHS rule
                       putStrLn "rhs"
                       mostraNodos $ getRHS rule
                       print $ INRule.getMapping rule
                       putStrLn "-----------------------------------"

paintHandler :: (InfoKind n g, InfoKind e g) =>
                State g n e -> DC () -> ActiveCanvas -> IO ()
paintHandler state dc canvas =
  do{ pDoc    <- getDocument       state
    ; doc     <- PD.getDocument    pDoc
    ; dp      <- getDisplayOptions state
    ; let network   = selectNetwork doc canvas
          selection = getSelection doc
          palette   = getPalette doc
          selection' = selection `filterSelectionTo` canvas
    ; mapp <- case canvas of
                   Net -> return []
                   LHS rule -> maybe (fail $ rule ++ " not found.")
                                     (return . map fst . getMapping)
                                     . findRule rule $ getRules doc
                   RHS rule -> maybe (fail $ rule ++ " not found.")
                                     (return . map snd . getMapping)
                                     . findRule rule $ getRules doc

    ; drawCanvas network palette selection' mapp dc dp
    }
  where filterSelectionTo :: Document.Selection -> ActiveCanvas
                             -> Document.Selection
        filterSelectionTo selection canvas =
            case selection of
               NodeSelection canv _ _        | canv == canvas -> selection
               EdgeSelection canv _          | canv == canvas -> selection
               ViaSelection  canv _ _        | canv == canvas -> selection
               MultipleSelection canv _ _ _  | canv == canvas -> selection
               _                                              -> NoSelection


chooseNetwork :: State g n e -> IO (Network g n e)
chooseNetwork state =
   do canvas <- getActiveCanvas state
      pDoc   <- getDocument     state
      doc    <- PD.getDocument  pDoc
      case canvas of
         Net      -> return $ getNetwork doc
         LHS rule -> maybe (fail $ "Invalid rule name: " ++ rule) return
                      $ getLHS `fromRule` rule $ getRules doc
         RHS rule -> maybe (fail $ "Invalid rule name: " ++ rule) return
                      $ getRHS `fromRule` rule $ getRules doc

mouseEvent :: (InfoKind n g, InfoKind e g, Show g, Parse g) =>
              EventMouse -> ScrolledWindow () -> Frame () -> State g n e -> IO ()
mouseEvent eventMouse canvas theFrame state = case eventMouse of
    MouseLeftDown mousePoint mods
        | shiftDown mods    -> leftMouseDownWithShift mousePoint state
        | metaDown mods || controlDown mods -> leftMouseDownWithMeta mousePoint state
        | otherwise         -> mouseDown True mousePoint theFrame state
    MouseRightDown mousePoint _ ->
        mouseDown False mousePoint theFrame state
    MouseLeftDrag mousePoint _ ->
        leftMouseDrag mousePoint canvas state
    MouseLeftUp mousePoint _ ->
        leftMouseUp mousePoint state
    _ ->
        return ()

keyboardEvent :: (InfoKind n g, InfoKind e g) =>
                 Frame () -> State g n e -> EventKey -> IO ()
keyboardEvent theFrame state (EventKey theKey _ _) =
    case theKey of
        KeyDelete                       -> deleteKey state
        KeyBack                         -> backspaceKey state
        KeyF2                           -> f2Key theFrame state
        KeyChar 'r'                     -> pressRKey theFrame state
        KeyChar 'i'                     -> pressIKey theFrame state
        KeyUp                           -> upKey state
        KeyDown                         -> downKey state
        _                               -> propagateEvent

closeDocAndThen :: State g n e -> IO () -> IO ()
closeDocAndThen state action =
  do{ pDoc <- getDocument state
    ; continue <- PD.isClosingOkay pDoc
    ; when continue $ action
    }

setInterfacePalette :: (InfoKind n g) => n -> State g n e -> IO ()
setInterfacePalette n state =
  do pDoc <- getDocument state

     let interfacePal = Palette [interfaceSymbol]
     -- set the initial palette only with interface symbol
     PD.superficialUpdateDocument (setPalette interfacePal) pDoc
     setCurrentShape (fst interfaceSymbol) state

     buildVisiblePalette state


newItem :: (InfoKind n g, InfoKind e g) => State g n e -> g -> n -> e -> IO ()
newItem state g n e =
    closeDocAndThen state $
      do{ pDoc <- getDocument state
        ; PD.resetDocument Nothing (Document.empty g n e) pDoc
        ; initializeRules state g n e
        ; reAddRules2Tree state
        ; setInterfacePalette n state
        ; repaintAll state
        }

openItem :: (InfoKind n g, InfoKind e g, XmlContent g) =>
            Frame () ->  State g n e -> IO ()
openItem theFrame state =
  do{ mbfname <- fileOpenDialog
        theFrame
        False -- change current directory
        True -- allowReadOnly
        "Open File"
        extensions
        "" "" -- no default directory or filename
    ; ifJust mbfname $ \fname -> openNetworkFile fname state (Just theFrame)
    }

-- Third argument: Nothing means exceptions are ignored (used in Configuration)
--              Just f means exceptions are shown in a dialog on top of frame f
openNetworkFile :: (InfoKind n g, InfoKind e g, XmlContent g) =>
                   String -> State g n e -> Maybe (Frame ()) -> IO ()
openNetworkFile fname state exceptionsFrame =
  closeDocAndThen state $
  flip catch
    (\exc -> case exceptionsFrame of
                Nothing -> return ()
                Just f  -> errorDialog f "Open network"
                    (  "Error while opening '" ++ fname ++ "'. \n\n"
                    ++ "Reason: " ++ show exc)
    ) $
  do{ contents <- strictReadFile fname
    ; let errorOrDocument = DocumentFile.fromString contents
    ; case errorOrDocument of {
        Left err -> ioError (userError err);
        Right (doc, warnings, oldFormat) ->
  do{
    ; pDoc <- getDocument state
    ; PD.resetDocument (if null warnings then Just fname else Nothing)
                       doc pDoc
    ; applyCanvasSize state
    ; when (not (null warnings)) $
        case exceptionsFrame of
            Nothing -> return ()
            Just f ->
              do{ errorDialog f "File read warnings"
                    (  "Warnings while reading file " ++ show fname ++ ":\n\n"
                    ++ unlines (  map ("* " ++) (take 10 warnings)
                               ++ if length warnings > 10 then ["..."] else []
                               )
                    ++ unlines
                    [ ""
                    , "Most likely you are reading a file that is created by a newer version of " ++ toolName ++ ". If you save this file with"
                    , "this version of " ++ toolName ++ " information may be lost. For safety the file name is set to \"untitled\" so that you do"
                    , "not accidentally overwrite the file"
                    ]
                    )
                ; PD.setFileName pDoc Nothing
                }
    ; when oldFormat $
          do{ case exceptionsFrame of
                Nothing -> return ()
                Just f ->
                    errorDialog f "File read warning" $
                       unlines
                       [ "The file you opened has the old " ++ toolName ++ " file format which will become obsolete in newer versions of " ++ toolName ++ "."
                       , "When you save this network, the new file format will be used. To encourage you to do so the network has"
                       , "been marked as \"modified\"."
                       ]
            ; PD.setDirty pDoc True
            }
    ; -- Redraw
    ; buildVisiblePalette state
    ; reAddRules2Tree state
    ; repaintAll state
    }}}

openPalette :: (InfoKind n g, Parse n) => Frame () ->  State g n e -> IO ()
openPalette theFrame state =
  do{ mbfname <- fileOpenDialog
        theFrame
        False -- change current directory
        True -- allowReadOnly
        "Open File"
        paletteExtensions
        "" "" -- no default directory or filename
    ; ifJust mbfname $ \fname -> openPaletteFile fname state (Just theFrame)
    }

-- Third argument: Nothing means exceptions are ignored (used in Configuration)
--              Just f means exceptions are shown in a dialog on top of frame f
openPaletteFile :: (InfoKind n g, Parse n) =>
                   String -> State g n e -> Maybe (Frame ()) -> IO ()
openPaletteFile fname state exceptionsFrame =
  flip catch
    (\exc -> case exceptionsFrame of
                Nothing -> return ()
                Just f  -> errorDialog f "Open shape palette"
                    (  "Error while opening '" ++ fname ++ "'. \n\n"
                    ++ "Reason: " ++ show exc)
    ) $
  do{ contents <- readFile fname
    ; case fst (runParser parse contents) of {
        Left msg -> ioError (userError ("Cannot parse shape palette file: "
                                       ++fname++"\n\t"++msg));
        Right p  -> do{ pDoc <- getDocument state
                      ; doc  <- PD.getDocument pDoc
                      ; let newPalette =  removeQuotesFromNames p
                            oldPalette = getPalette doc
                            newNames = shapesNames newPalette
                            oldNames = shapesNames oldPalette

                      ; let cont = newNames `union` oldNames == newNames
                      ; yes <- if cont
                                 then return True
                                 else
                                   case exceptionsFrame of
                                     Nothing -> return False
                                     Just f -> confirmDialog f
                                                  "Conflict with palettes"
                                                  ("The old palette has names that are not defined in the one that you intend to load.\nThis can make the system inconsistent.\n\nDo you want to load the new palette anyway ?") False
                      ; when (cont || yes) $
                            do PD.updateDocument "change palette"
                                                 (setPalette newPalette)
                                                 pDoc
                               -- the shape name of the first palette's element is chosen
                               -- as the default one
                               setCurrentShape (fst  . head . shapes $ newPalette) state

                               buildVisiblePalette state
                      }
    }}
   where remQuot = init . tail
         removeQuotesFromNames = Palette . map (\(a,b) -> (remQuot a, rem1 b) ) . shapes
         rem1 (shape, mPorts, info) = (shape, map rem3 mPorts , info)
         rem3 (str, dpoint) = (remQuot str, dpoint)

savePalette :: Show n => Frame () -> State g n e -> IO ()
savePalette theFrame state =
  do pDoc <- getDocument state
     doc  <- PD.getDocument pDoc
     mfname <- PD.defaultSaveAsDialog theFrame paletteExtensions Nothing
     case mfname of
        Just fname -> do safeWriteFile theFrame fname . show $ getPalette doc
                         return ()
        Nothing -> return ()

-- | Get the canvas size from the network and change the size of
--   the widget accordingly
applyCanvasSize :: State g n e -> IO ()
applyCanvasSize state =
  do{ pDoc <- getDocument state
    ; doc <- PD.getDocument pDoc
    ; let network = getNetwork doc
          (width, height) = getCanvasSize network
    ; canvas <- getCanvas state
    ; ppi <- getScreenPPI
    ; set canvas [ virtualSize := sz (logicalToScreenX ppi width)
                                     (logicalToScreenY ppi height) ]
    }


saveToDisk :: (InfoKind n g, InfoKind e g, XmlContent g) =>
              Frame () -> String -> Document.Document g n e -> IO Bool
saveToDisk theFrame fileName doc =
    safeWriteFile theFrame fileName (DocumentFile.toString doc)

exit :: State g n e -> IO ()
exit state =
    closeDocAndThen state $ propagateEvent

-- Code for build the Visible Palette

buildVisiblePalette :: InfoKind n g => State g n e -> IO ()
buildVisiblePalette state =
  do{ pDoc2 <- getDocument state
    ; pp    <- getPalettePanel state
    ; doc   <- PD.getDocument pDoc2
    ; let palette = getPalette doc

    -- its necessary to delete the old elements in the panel
    ; windowChildren pp >>= mapM objectDelete

    ; reallyBuildVisiblePalette palette pp state setCurrentShape
    }

reallyBuildVisiblePalette :: InfoKind n g =>
                             Palette.Palette n -> Panel () -> State g n e
                             -> (String -> State g n e -> IO ()) -> IO ()
reallyBuildVisiblePalette palette panel state action =
    do list  <- mapM (drawNodeButton panel state action) . shapes $ palette
       let table = list2Table 2 list

       set panel [layout :=
#if !defined(__APPLE__)
                            boxed "Symbol palette"
#endif
                              (grid 4 4 table) ]


drawNodeButton :: InfoKind n g => Window w -> State g n e -> (String -> State g n e -> IO ())
                  -> (String, (Shape, Ports, Maybe n)) -> IO Layout
drawNodeButton w state action (name, (shape, ports, _info)) =
  do{ frame <- getNetworkFrame state
    ; node  <- button w [ text := name
                        -- , clientSize := sz 50 50  -- due to a wxHaskell problem forcing the size don't works
                        , on command := action name state
                        , on mouse := \ev -> safetyNet frame $ mouseSymbol ev name action state
                        , tooltip := name
                        , bgcolor := white
                        , on paint := \dc r -> safetyNet frame $
                                         do { logicalDraw ppi dc (center r) shape []
                                            ; drawPorts ppi dc (center r) ports []
                                            }
                        -- , checked := True
                        ]
    ; return (widget node)
    }
    where factor f (DoublePoint x y) = DoublePoint (x/f) (y/f)
          ppi = (sz 40 40) -- (rectSize r)   this is the correct code if
          center r = factor 14.0 $ intPointToDoublePoint $ rectCentralPoint r

mouseSymbol :: InfoKind n g => EventMouse -> String -> (String -> State g n e -> IO ()) -> State g n e -> IO ()
mouseSymbol mouseEV name action state =
    case mouseEV of
        MouseLeftUp  _ _ -> action name state
        MouseRightUp _ _ -> removeSymbolUI name state
        _ -> return ()

-- | Transforms a list in a table of n columns
list2Table :: Int -> [a] -> [[a]]
list2Table n l | null l    = []
               | otherwise = a : list2Table n b
               where (a,b) = splitAt n l

createNewAgentItem :: InfoKind n g => State g n e -> IO ()
createNewAgentItem state =
   do mRes <- createAgentByNameDialog state
      case mRes of
          Just (agentName, agentShape, ports) ->
               do{ pDoc <- getDocument state
                 ; let newElem = (agentName, (agentShape, ports, Nothing))
                 ; PD.updateDocument "change palette"
                      (\doc -> setPalette (Palette . (++ [newElem]) . shapes . getPalette $ doc)
                                          doc)
                      pDoc

                 ; setCurrentShape (agentName) state

                 ; buildVisiblePalette state
                 }
          _ -> return ()

removeSymbolUI :: (InfoKind n g) => String -> State g n e -> IO ()
removeSymbolUI name state =
  when (name /= "interface") $
     do contextMenu <- menuPane []
        menuItem contextMenu
                [ text := "Remove symbol"
                , on command := removeSymbol name state
                ]
        theFrame <- getNetworkFrame state
        pointWithinWindow <- windowGetMousePosition theFrame
        menuPopup contextMenu pointWithinWindow theFrame
        objectDelete contextMenu

-- | Removes a symbol if it is not used or else don't remove and list its occurrences.
removeSymbol :: (InfoKind n g) => String -> State g n e -> IO ()
removeSymbol name state =
  do theFrame <- getNetworkFrame state
     if name == "interface"
      then warningDialog theFrame "Deletion forbidden" "Interface symbol is a special one that can't be deleted."
      else
      do
        pDoc <- getDocument state
        doc  <- PD.getDocument pDoc

        let newpal = deleteShape name $ getPalette doc
            symbs = map fst . shapes $ newpal
            errors = undefinedAgents symbs doc
        if Map.null errors
         then do -- symbol can be safely removed
                remove <- confirmDialog theFrame "Symbol deletion"
                                ("Symbol \"" ++ name ++
                                "\" can be safely removed.\nAre you sure you want to delete it?")
                             False
                if remove
                 then do PD.updateDocument ("Symbol " ++ name ++ " removed") (setPalette newpal) pDoc
                         buildVisiblePalette state
                 else return ()

         else -- there are occurrences of symbol; list them
              errorDialog theFrame "Deletion forbidden" $
                "Symbol \"" ++ name ++
                "\" can't be deleted because there are the following occurrences of it in the IN system:\n"
                ++ (unlines . map show $ Map.keys errors)

-- | List the rules on a one level tree.
addRules2Tree :: (InfoKind.InfoKind n g, InfoKind.InfoKind e g) =>
                 TreeCtrl a -> TreeItem -> State g n e -> IO ()
addRules2Tree tree item state =
   do pDoc <- getDocument state
      doc  <- PD.getDocument pDoc

      treeCtrlDeleteChildren tree item
      let rNames = rulesNames $ getRules doc
      mapM_ addItemRule rNames

      -- choose the last rule as the active (displayed) one
      rule     <- treeCtrlGetLastChild tree item
      ruleName <- treeCtrlGetItemText tree rule
      setActiveRule ruleName state
      treeCtrlSelectItem tree rule
   where addItemRule ruleName =
            treeCtrlAppendItem tree item ruleName noImage noImage objectNull

-- | Eliminates old rules and add the newer ones.
reAddRules2Tree :: (InfoKind.InfoKind n g, InfoKind.InfoKind e g) =>
                   State g n e -> IO ()
reAddRules2Tree state =
    do tree <- getTree state
       root <- treeCtrlGetRootItem tree
       addRules2Tree tree root state

onTreeEvent :: (InfoKind.InfoKind n g, InfoKind.InfoKind e g) =>
               TreeCtrl a -> State g n e -> g -> n -> e -> EventTree -> IO ()
onTreeEvent tree state g n e event =
  case event of
      TreeSelChanged item olditem | treeItemIsOk item
        -> do wxcBeginBusyCursor
              ruleName <- treeCtrlGetItemText tree item
              when (ruleName /= "Rules") $
                   do setActiveRule ruleName state
                      repaintAll state
              wxcEndBusyCursor
              propagateEvent
      TreeBeginLabelEdit item str action
           | str == "Rules" -> action -- prevents the root from be editable
      TreeEndLabelEdit item new wasCanceled veto | not wasCanceled ->
           do -- change rule name
              old <- treeCtrlGetItemText tree item

              when (new /= old) $
                  do pDoc  <- getDocument state
                     frame <- getNetworkFrame state
                     doc   <- PD.getDocument pDoc
                     let rNames = rulesNames $ getRules doc
                     if new `elem` rNames
                       then do veto
                               warningDialog frame "Warning"
                                  $ "Already exists one rule with name \"" ++ new
                                    ++ "\".\n Please choose a different identifier."
                       else do PD.updateDocument "change rule name"
                                   (updateRules
                                      $ updateRule old
                                         $ INRule.setName new) pDoc
                               setActiveRule new state
              propagateEvent
      TreeItemRightClick item ->
           do ruleName <- treeCtrlGetItemText tree item
              contextMenu <- menuPane []
              theFrame    <- getNetworkFrame state

              if (ruleName == "Rules") -- means right click on root item
                then
                  do menuItem contextMenu
                       [ text := "Add new rule"
                       , on command := safetyNet theFrame $ addNewRuleItem True state $ initial g n e
                       ]
                     menuItem contextMenu
                       [ text := "Create new Interaction Net rule"
                       , on command := safetyNet theFrame $ createRuleItem theFrame state g n e
                       ]
                else
                  do menuItem contextMenu
                       [ text := "Rename rule"
                       , on command := treeCtrlEditLabel tree item
                       ]
                     menuItem contextMenu
                       [ text := "Remove rule"
                       , on command := do safetyNet theFrame
                                              $ removeRuleItem state ruleName item
                                          propagateEvent
                       ]

              propagateEvent
              pointWithinWindow <- windowGetMousePosition theFrame
              menuPopup contextMenu pointWithinWindow theFrame
              objectDelete contextMenu
      _
        -> propagateEvent

-- | Adds a new rule setting it with a new name.
addNewRuleItem :: (InfoKind.InfoKind n g, InfoKind.InfoKind e g) =>
                Bool  -> State g n e -> INRule g n e -> IO ()
addNewRuleItem genNewName state newRule =
    do pDoc <- getDocument state
       doc <- PD.getDocument pDoc

       let newName = if genNewName then (addNew 1 . rulesNames $ getRules doc) else (INRule.getName newRule)

       PD.updateDocument ("add rule <<" ++ newName ++ ">>")
            (updateRules $ addNewRule $ (if genNewName then INRule.setName newName else id) $ newRule ) pDoc

       tree <- getTree state
       root <- treeCtrlGetRootItem tree
       item <- treeCtrlAppendItem tree root newName noImage noImage objectNull
       treeCtrlSelectItem tree item
    where addNew :: Int -> [String] -> String
          addNew i rules | newName `elem` rules = addNew (i+1) rules
                         | otherwise = newName
                         where newName = "Rule " ++ show i
updateTreeSelection :: State g n e -> IO ()
updateTreeSelection state =
   do tree    <- getTree state
      root    <- treeCtrlGetRootItem tree
      selItem <- treeCtrlGetLastChild tree root
      treeCtrlSelectItem tree selItem

removeRuleItem :: (InfoKind n g, InfoKind e g) =>
                  State g n e -> String -> TreeItem -> IO ()
removeRuleItem state ruleName item =
   do frame <- getNetworkFrame state
      pDoc  <- getDocument state
      doc   <- PD.getDocument pDoc

      if (1 == ) . length . rulesNames $ getRules doc
         then warningDialog frame "Removal forbidden"
                "You cannot remove the rule because it is the last one."
         else do tree  <- getTree state
                 delete <- confirmDialog frame "Rule deletion" msg yesDefault

                 when (delete) $
                     do treeCtrlDelete tree item
                        updateTreeSelection state

                        PD.updateDocument ("remove rule " ++ ruleName)
                            (updateRules $ removeRule ruleName) pDoc

   where yesDefault = False
         msg = "Are you sure you want to delete rule \"" ++ ruleName ++ "\" ?"

-- | If there are none rules it creates a empty one.
initializeRules :: (InfoKind.InfoKind n g, InfoKind.InfoKind e g) =>
                   State g n e -> g -> n -> e -> IO ()
initializeRules state g n e =
   do pDoc <- getDocument state
      doc  <- PD.getDocument pDoc
      let rNames = rulesNames $ getRules doc

      if (null rNames)
       then
         do -- adds an initial rule
            PD.superficialUpdateDocument
                             (updateRules $ addNewEmptyRule "Rule 1" g n e) pDoc
            setActiveRule "Rule 1" state
       else setActiveRule (head rNames) state

lhs2rhsItem :: Bool -> State g n e -> IO ()
lhs2rhsItem everything state =
   do pDoc <- getDocument state
      rule <- getActiveRule state
      theFrame <- getNetworkFrame state
      doc  <- PD.getDocument pDoc
      let rhs = selectNetwork doc $ RHS rule

      copy <- if isEmpty rhs
                  then return True
                  else proceedDialog theFrame "Non empty RHS" $
                          "The RHS side of the rule is not empty.\n" ++
                          "Copying the LHS will make you loosing it.\n" ++
                          "Do you want to proceed ?"
      when copy $
         do if everything
              then PD.updateDocument ("copy of LHS to RHS on rule " ++ rule)
                      (updateRules $ updateRule rule $ copyLHS2RHS) pDoc
              else PD.updateDocument ("copy of LHS interface to RHS on rule " ++ rule)
                      (updateRules $ updateRule rule $ copyLHSInterface2RHS) pDoc
            repaintAll state
            setActiveCanvas (RHS rule) state

data CopyLHS2RHS = Everything | JustInterface | DontCopy | DefaultRule  deriving (Show)

-- | Create a dialog where the user have to choose two symbols.
-- An interaction net rule, whose left hand side is the active pair
-- of those two agents, will then be created.
-- A new name is created for this rule.
createRuleItem :: (InfoKind n g, InfoKind e g) =>
                  Frame () -> State g n e -> g -> n -> e -> IO ()
createRuleItem frame state g n e =
  do
     maybeRes <- chooseAgentsDialog state
     when (isJust maybeRes) $
         do{
                   ; pDoc <- getDocument state
           ; doc  <- PD.getDocument pDoc
                   ;let palette = getPalette doc
                   ;let ;(agent1, agent2, copyOption) = fromJust maybeRes
                                ;copy = case copyOption of
                             Everything    -> copyLHS2RHS
                             JustInterface -> copyLHSInterface2RHS
                             DontCopy      -> id
                             DefaultRule   -> defaultRuleSelector (agent1,agent2) state g n e  palette
           ; (rule,nNr1,nNr2) <- createRuleWizard g n e palette agent1 agent2
           ; addNewRuleItem False state $ copy rule
           }

-- An interaction net rule, whose left hand side is the active pair
-- of the two given agents, will be created.
-- A new name is created for this rule.
-- Empty RHS.
createRuleWizard :: (InfoKind n g, InfoKind e g) =>
                    g -> n -> e -> Palette n
                    -> String -> String -> IO (INRule g n e, NodeNr, NodeNr)
createRuleWizard g n e palette agent1 agent2 =
        do{
           ; let
                 (nNr1, lhs1) = addNode agent1 palette
                                         $ Network.empty g n e
                 (nNr2, lhs2) = addNode agent2 palette lhs1

           ; (pP1:ports1) <- getPorts' agent1 palette
           ; (pP2:ports2) <- getPorts' agent2 palette

                 -- edge connecting principal ports
           ; let lhs3 = addEdge palette nNr1 (fst pP1) nNr2 (fst pP2) lhs2

                 (pos1, pos2) = givePositions pP1 pP2 -- (DoublePoint 2.0 2.0, DoublePoint 6.0 3.0) -- ??
                 lhs4 = setNodePosition nNr1 pos1
                        . setNodePosition nNr2 pos2 $ lhs3

                 -- adding as many interface nodes as needed
                 (nrs1, lhs5) = addNodes (fst interfaceSymbol) palette (length ports1) lhs4
                 (nrs2, lhs6) = addNodes (fst interfaceSymbol) palette (length ports2) lhs5

           ; interPort <- getInterfacePort palette

                 -- choose interface agents better positions; up or down
           ; let (ups1, downs1) = (map snd >< map snd ) . partition sep $ zip ports1 nrs1
                 (ups2, downs2) = (map snd >< map snd ) . partition sep $ zip ports2 nrs2
                 orderConcat = chooseOrder pos1 pos2

                 -- add edges between not principal ports in agents to interface nodes and set their positions
                 lhs7 = rowNodes (DoublePoint 0.5 5.5) (orderConcat downs1 downs2)
                        . rowNodes  (DoublePoint 0.5 0.5) (orderConcat ups1 ups2)
                        . addEdges palette -- edges agent2 to interface
                            [((nNr2, fst p'), (n', fst interPort)) | p' <- ports2 | n' <- nrs2]
                        . addEdges palette -- edges agent1 to interface
                            [((nNr1, fst p'), (n', fst interPort)) | p' <- ports1 | n' <- nrs1] $ lhs6
           ; let rhs = Network.empty g n e
                 mapping = []
           ; return (construct (agent1 ++ "_" ++ agent2) lhs7 rhs mapping, nNr1,nNr2)
           }
  where getPorts' = getSymbolPorts

        getInterfacePort :: Palette.Palette n -> IO Port
        getInterfacePort palette =
           do ps <- getPorts' "interface" palette
              case ps of
                [port] -> return port
                _      -> fail "Interface agent with more than one port."
        givePositions :: Port -> Port -> (DoublePoint, DoublePoint)
        givePositions port1 port2 = g (portZone port1) (portZone port2)
         where g Ztop    Ztop    = lineH
               g Zbottom Zbottom = lineH
               g Zleft   Zleft   = lineV
               g Zright  Zright  = lineV
               g Ztop    Zbottom = invert lineV
               g Zbottom Ztop    = lineV
               g Zleft   Zright  = invert lineH
               g Zright  Zleft   = lineH
               g Ztop    Zleft   = invert lineI
               g Ztop    Zright  = invert lineD
               g Zbottom Zleft   = lineD
               g Zbottom Zright  = lineI
               g Zleft   Ztop    = lineI
               g Zleft   Zbottom = invert lineD
               g Zright  Ztop    = lineD
               g Zright  Zbottom = invert lineI

               c1 = 2.0
               c2 = 4.0
               p1 = DoublePoint c1 c1
               p2 = DoublePoint c2 c1
               p3 = DoublePoint c1 c2
               p4 = DoublePoint c2 c2
               lineH = (p1, p2)
               lineV = (p1, p3)
               lineD = (p1, p4)
               lineI = (p2, p3)
               invert = swap

        sep :: (Port, NodeNr) -> Bool
        sep (port, _) = isUp port
        chooseOrder pos1 pos2 = if doublePointX pos1 <= doublePointX pos2
                                  then (++)
                                  else flip (++)

getSymbolPorts :: String -> Palette.Palette n -> IO Ports
getSymbolPorts shape (Palette palette) =
            case Data.List.lookup shape palette of
              Nothing -> fail $ shape ++ " agent is missing."
              Just e  -> case snd3 e of
                          [] -> fail $ shape ++ " agent without port."
                          ps -> return ps

-- | For a list of nodes placed at the same position, change their positions to form a row.
rowNodes :: DoublePoint -> [NodeNr] -> Network g n e -> Network g n e
rowNodes startingPoint nodes net = snd $ foldl gene (startingPoint, net) nodes
gene :: (DoublePoint, Network g n e) -> NodeNr -> (DoublePoint, Network g n e)
gene (actual, oldNet) nNr = (translate actual diff, setNodePosition nNr actual oldNet)
diff = DoublePoint 1.0 0.0




defaultRuleSelector  :: (InfoKind n g, InfoKind e g) => (String,String) ->  State g n e-> g ->n -> e ->
                                                        Palette.Palette n -> INRule g n e -> INRule g n e
defaultRuleSelector (a1,a2) state g n e  pal rule     = case a2 of
                                                          "copy"       -> copyORduplicatorDefaultRule (a1,a2) rule state g n e pal
                                                          "duplicator" -> copyORduplicatorDefaultRule (a1,a2) rule state g n e pal
                                                          "Erase"      -> eraseDefaultRule a1 rule state g n e pal
                                                          _            -> rule

------------------------------------------------------------------------------------------------------------------------

eraseDefaultRule ::  (InfoKind n g, InfoKind e g) => String ->INRule g n e -> State g n e -> g -> n -> e -> Palette.Palette n -> (INRule g n e)
eraseDefaultRule a1 rule state g n e palette =
                                        let ;newRule = copyLHSInterface2RHS  $  construct "" lhs (Network.empty g n e) []
                                                                                        ;rhs = getRHS newRule
                                                                                        ;inter = getNodeAssocs rhs
                                                                                        ;(erasers, rhs1) = addNodes "Erase" palette (length inter) rhs
                                                                                        ;rhs2 = rowNodes (DoublePoint 1.0 1.0) erasers rhs1
                                                                                        ;rhs3 = addEdges palette (mix (map fst inter) erasers) rhs2
                                                                                  in (construct  ("T:Erase_"++a1) lhs rhs3 (getMapping newRule))

 where
        mix [] [] = []
        mix (x:y ) (a:b) = [((x, "interface"),(a, "down"))  ]++ mix y b
        mix _ _ = []
                ;lhs = getLHS rule





copyORduplicatorDefaultRule :: (InfoKind n g, InfoKind e g) => (String,String) -> INRule g n e ->
                                State g n e -> g -> n -> e ->  Palette.Palette n -> (INRule g n e)
copyORduplicatorDefaultRule (a1,a2) rule state g n e palette = let
                                                newRule = copyLHSInterface2RHS  $  construct "" lhs (Network.empty g n e) []
                                                rhs     = getRHS newRule
                                                inter = getNodeAssocs rhs
                                                (alphas, rhs1) = addNodes a1 palette 2 rhs
                                                (spas,rhs2)    = addNodes a2 palette ((\x -> x-1) . length . snd3 . takeJust "Undefined symbol" $ getSymbol a1 palette) rhs1
                                                rhs3 = rowNodes (DoublePoint 1.0 4.0) spas $ (rowNodes (DoublePoint 1.0 2.0) alphas rhs2)
                                                (copyI,alphaI) = splitAt ((length inter)-2) inter
                                                rhs4 = makeInterfaceConection (map fst alphaI) alphas $ makeInterfaceConection (map fst (reverse copyI)) spas rhs3
                                                newC :: [((NodeNr, PortName), (NodeNr, PortName))]
                                                newC = newConections $ makeConnection alphas spas  rhs4
                                                rhs5 = addEdges palette newC rhs4
                                                                                        in (construct  ('T':':':a2++'_':a1) lhs rhs5 (getMapping newRule))
                    where ;lhs = getLHS rule
--                        ;makeInterfaceConection :: [NodeNr] -> [NodeNr] -> Network g n e -> Network g n e
                          ;makeInterfaceConection inter node ne = let pp_i = getInterfaceList inter ne
                                                                      pp_n = getInterfaceList node ne
                                                                   in addEdges palette (zip  (zip inter pp_i) (zip node pp_n))  ne
                          ;getInterfaceList nodes ne = map (const "interface") nodes
--                        ; getJustPorts :: Network g n e -> NodeNr -> [PortName]
                          ; getJustPorts ne y = map fst . fromJust $ getPorts palette ne y

--                        ;makeConnection :: [NodeNr] -> [NodeNr] -> Network g n e ->([((NodeNr,PortName),(Int,Int))] , [((NodeNr,PortName),(Int,Int))])
                          ;makeConnection alphs sps ne = let
                                                            ;alp_p :: [[PortName]]
                                                            ;alp_p = map (reverse . drop 1 . getJustPorts ne ) alphs
                                                            ;alp_p_i = map (zip [1..] ) alp_p
                                                            ;alp_Nr_p = zip [1..] alp_p_i
                                                            ;alp_fin = zip alphs alp_Nr_p
                                                            ;sps_p = map (drop 1 . getJustPorts ne) sps
                                                            ;sps_p_i = map (zip [1..] ) sps_p
                                                            ;sps_Nr_p = zip [1..] sps_p_i
                                                            ;sps_fin = zip sps sps_Nr_p
                                                          in (foldr (++)  [] $ map f alp_fin  , foldr (++)  [] $ map f sps_fin)
                          ;f :: (Int,(Int,([(Int,PortName)]))) -> [((NodeNr,PortName),(Int,Int))]
                          ;f (_,(_ ,([]))) = []
                          ;f(nodeNr,(node_i,((port_i,port) : l ))) = [((nodeNr,port),(node_i,port_i))] ++ f (nodeNr,(node_i,( l )))
getOther :: [((NodeNr,PortName),(Int,Int))] -> (Int,Int) -> (NodeNr,PortName)
getOther (((nr,p),(ni,pi)) :l) (a,b) | (b == ni) && (a==pi) = (nr,p)
                                     | otherwise = getOther l (a,b)
newConections :: ([((NodeNr,PortName),(Int,Int))],[((NodeNr,PortName),(Int,Int))]) ->[((NodeNr,PortName),(NodeNr,PortName))]
newConections ([],_)  = []
newConections ((((nr,p),(ni,pi)) :l), ll) = [((nr,p), getOther ll (ni,pi) ) ] ++ (newConections  (l,ll))


chooseAgentsDialog :: InfoKind n g => State g n e
                      -> IO ( Maybe (String, String, CopyLHS2RHS))
chooseAgentsDialog state =
    do theFrame <- getNetworkFrame state
       pDoc <- getDocument state
       doc <- PD.getDocument pDoc

       -- palette without interface symbol
       let pal = filter ( (/= fst interfaceSymbol).fst )
                       . shapes $ getPalette doc

       if null pal
         then
           do warningDialog theFrame "No symbols" "There are no symbols other than interface one.\nAdd symbol first."
              return Nothing
         else
           do let palette = Palette pal
              -- no button was pressed
              setShape1 Nothing state
              setShape2 Nothing state

              -- create Dialog
              dia <- dialog theFrame [ text := "Rule creation wizard"]
              p  <- panel dia []
              p1 <- panel p []
              p2 <- panel p []
                          ;p3 <- panel p2 []

              ok <- button p [ text := "Ok"
                      , enabled := False
                      ]
              setOkButton ok state

              let rinfo = [ ("all nodes", Everything)
                          , ( "just interface nodes", JustInterface)
                          , ("nothing", DontCopy)
                                                  , ("Rule template",DefaultRule)
                          ]
                  (rlabels, rdata) = unzip rinfo
              r1 <- radioBox p Vertical rlabels
                      [ text := "What to copy automatically from LHS to RHS ?"
                      , selection := 1

                                          ]

              ca <- button p [ text := "Cancel" ]

                          ;set r1  [ on select ::= logSelect pal p2  state (onClick r1 setJustShape2)]


              reallyBuildVisiblePalette palette p1 state $ onClick r1 setJustShape1
              reallyBuildVisiblePalette palette p2 state $ onClick r1 setJustShape2


              set dia [ layout := container p $
                                    margin 10 $
                                      column 5 [ label "Choose one symbol in each palette."
                                               , widget p1
                                                                                           , hrule 350
                                               , widget p2
                                               , widget r1
                                               , row 5 [widget ok, widget ca]
                                               ]
                      ]


              showModal dia $ \stop ->
                do set ok   [on command :=
                                     do  mAgent1 <- getShape1 state
                                         mAgent2 <- getShape2 state
                                         i <- get r1 selection
                                         let res = (fromJust mAgent1, fromJust mAgent2, rdata !! i)

                                         stop (Just res) ]
                   set ca   [on command := stop Nothing ]

    where setJustShape1 = setShape1 . Just
          setJustShape2 = setShape2 . Just
          onClick r1 func name state =
              do func name state
                 mAgent1  <- getShape1 state
                 mAgent2  <- getShape2 state
                 i  <- get r1 selection
                 okButton <- getOkButton state
                 set okButton [ enabled := if (i==3) then (((fromMaybe "" mAgent2) `elem` ["Erase","duplicator","copy"] ) && isJust mAgent1)
                                                                     else (isJust mAgent1 && isJust mAgent2)
                                                          ]
          logSelect pal p2 state f w
           = do ;i <- get w selection
                        ;mAgent2  <- getShape2 state
                                ;mAgent1  <- getShape1 state
                                ;okButton <- getOkButton state
                    ;if (i == 3)
                         then do ;set okButton [enabled :=  (fromMaybe "" mAgent2) `elem` ["Erase","duplicator","copy"] ]
                                         ;let specialPalette =  filter (\x -> (fst x) == "Erase" || (fst x) == "duplicator" || (fst x) == "copy"  ) pal
                                     ;if (null specialPalette)
                                                   then do  ;theFrame <- getNetworkFrame state
                                                                        ;errorDialog theFrame "Not Defined" "No rules defined for any of the symbols in the pallete"
                                                                ;set w [selection := 1]
                                               else do{ ; windowChildren p2 >>= mapM objectDelete
                                                ; n <- panel p2 []
                                                        ;reallyBuildVisiblePalette (Palette specialPalette) n state f
                                                      }
                                 else do {;set okButton [enabled :=  (isJust mAgent1 && isJust mAgent2) ]
                                          ; windowChildren p2 >>= mapM objectDelete
                                      ; n <- panel p2 []
                                              ;reallyBuildVisiblePalette (Palette pal)  n state f
                                                 }





createAgentByNameDialog :: State g n e -> IO (Maybe (String, Shape, [Port]))
createAgentByNameDialog state =
    do theFrame <- getNetworkFrame state
       pDoc <- getDocument state
       doc <- PD.getDocument pDoc

       -- palette shape names
       let paletteNames = map fst . shapes $ getPalette doc

       -- create Dialog
       diaW <- dialog theFrame [ text := "Create new symbol"
--                              , visible := True
                              , resizeable := True
                              , clientSize := sz 200 300
                              ]
       p  <- panel diaW []
       nb <- notebook p []

       p1 <- panel nb []
       agent1 <- entry p1 [text := "Symbol name"]
 --      set agent [ on keyboard := \k -> do propagateEvent
 --                                          agentD <- get agent text
 --                                          if agentD `elem` paletteNames
 --                                          then set agent [bgcolor := red]
 --                                          else set agent [bgcolor := green] ]
       symb1  <- entry p1 [text := "Displayed name" ]
 --      set symb [ on keyboard := \k -> do propagateEvent
 --                                         set symb [ bgcolor := green ]
 --                                         repaint agentG
       portsC1 <- textCtrl p1 [text := "[ (\"port_name\", DoublePoint 0.3 (-0.3))\n]" ]
 --      set portsC [ on keyboard := \k -> do propagateEvent
 --                                           portsT <- get portsC text
 --                                           case (reads :: ReadS [Port] ) portsT of
 --                                               [(ports,"")] -> do set portsC [ bgcolor := green]
 --                                                                  repaint agentG
 --                                               _ -> set portsC [ bgcolor := red]

       p2 <- panel nb []
       agent2  <- entry p2 [text := "Symbol name"]
       symb2   <- entry p2 [text := "Displayed name" ]
       portsC2 <- textCtrl p2 [text := "[ (\"port_name\", DoublePoint 0.3 (-0.3))\n]" ]

       p3 <- panel nb []
       symbs3 <- radioBox p3 Vertical (map fst $ shapes managementSymbols)
                   [ text := "Management symbols:"
                   , selection := 1
                   ]

       let (width, height) = (10,10)
       ppi <- getScreenPPI
       agentG <- window p
        [ virtualSize   := sz (logicalToScreenX ppi width)
                              (logicalToScreenY ppi height)
        , clientSize := sz 300 50
        , fullRepaintOnResize := False
                          , bgcolor := wxcolor paneBackgroundColor
                          , on paint := \dc r -> safetyNet theFrame $
                                do page <- notebookGetSelection nb
                                   case page of
                                    0 ->  -- Standard symbol
                                         do { symbD <- get symb1 text
                                            ; portsT <- get portsC1 text

                                            ; case (reads :: ReadS [Port] ) portsT of
                                                [(ports,"")] ->
                                                  if haveRepeateds (map fst ports)
                                                        then logMessage "port names repeated"
                                                        else
                                                                do let shape =
                                                                          TextInEllipse { shapeStyle = defaultShapeStyle
                                                                                        , shapeText = symbD}

                                                                   drawFig dc r shape ports []

                                                _ -> logMessage "bad parsing in ports" -- return ()
                                            }
                                    1 ->  -- Syntactical symbol
                                         do { symbD <- get symb2 text
                                            ; portsT <- get portsC2 text
                                            ; case (reads :: ReadS [Port] ) portsT of
                                                [(ports,"")] ->
                                                  if haveRepeateds (map fst ports)
                                                        then logMessage "port names repeated"
                                                        else
                                                                do let shape =
                                                                        Composite { shapeSegments =
                                                                                 [ Polygon
                                                                                        { shapeStyle = defaultShapeStyle
                                                                                        , shapePerimeter =
                                                                                           [ DoublePoint 0 (-0.7)
                                                                                           , DoublePoint (-0.7) 0.15
                                                                                           , DoublePoint 0.7 0.15
                                                                                           ]
                                                                                        }
                                                                                 , Text { shapeStyle = defaultShapeStyle
                                                                                        , shapeText = symbD} ]}
                                                                   drawFig dc r shape ports []
                                                _ -> logMessage "bad parsing in ports" -- return ()
                                            }
                                    2 ->  -- Management symbol
                                     do i <- get symbs3 selection
                                        let (shape, ports, _) = snd $ (shapes managementSymbols) !! i
                                        drawFig dc r shape ports []
                          ]
       set agentG [ on mouse :=    \p    -> repaint agentG
                  , on keyboard := \k    -> repaint agentG
                  ]

       test <- button p [text := "Test", on command := repaint agentG]

       ok <- button p [ text := "Ok"]
       ca <- button p [ text := "Cancel" ]

       set diaW [ layout := container p $
                             margin 10 $ fill $
                               column 5
                                [
                                  label "New symbol"
                                , tabs nb
                                   [ tab "Standard" . container p1 .
                                          fill $ grid 5 5
                                            [[label "Symbol name", hfill $ widget agent1]
                                            ,[label "Symbol displayed name", hfill $ widget symb1]
                                            ,[label "list of ports", hfill $ widget portsC1]]
                                   , tab "Syntactical" . container p2 .
                                          fill $ grid 5 5
                                            [[label "Symbol name", hfill $ widget agent2]
                                            ,[label "Symbol displayed name", hfill $ widget symb2]
                                            ,[label "list of ports", hfill $ widget portsC2]]
                                   , tab "Management" . container p3 . fill $ widget symbs3
                                   ]
                                , hfill $ widget test
                                , fill $ widget agentG
                                , floatBottomRight $ row 5 [widget ok, widget ca]
                                ]
               ]

       showModal diaW $ \stop ->
                do set ok   [on command :=
                        do page <- notebookGetSelection nb
                           case page of
                            0 ->  -- Standard symbol
                              do{ agentD <- get agent1 text
                                ; symbD  <- get symb1 text
                                ; portsT <- get portsC1 text

                                ; if agentD `elem` paletteNames
                                     then do { errorDialog diaW "Repeated symbol name" $ "Already exists one symbol with name \"" ++ agentD ++ "\". Choose a different one."
                                            -- ; set agent [bgcolor := red]
                                          }
                                     else
                                      if agentD `elem` map fst (shapes specialSymbols)
                                      then errorDialog diaW "Reserved agent name" $ "\"" ++ agentD ++ "\" is a reserved agent name for a special agent.\nPlease import the agent or choose a different name."
                                      else
                                       do let shape = TextInEllipse { shapeStyle = defaultShapeStyle
                                                                    , shapeText = symbD}
                                          case (reads :: ReadS [Port] ) portsT of
                                            [(ports,"")] ->
                                                  if haveRepeateds (map fst ports)
                                                        then errorDialog diaW "Bad Ports" "port names repeated"
                                                        else stop $ Just (agentD, shape, ports)
                                            _ -> do { errorDialog diaW "Parse error in list of Ports"                                                     "Parse error in list of Ports"
                                                   -- ; set portsC [ bgcolor := red ]
                                                    }
                                }
                            1 ->  -- Syntactical symbol
                              do{ agentD <- get agent2 text
                                ; symbD  <- get symb2 text
                                ; portsT <- get portsC2 text

                                ; if agentD `elem` paletteNames
                                     then do { errorDialog diaW "Repeated symbol name" $ "Already exists one symbol with name \"" ++ agentD ++ "\". Choose a different one."
                                            -- ; set agent [bgcolor := red]
                                          }
                                     else
                                      if agentD `elem` map fst (shapes specialSymbols)
                                      then errorDialog diaW "Reserved agent name" $ "\"" ++ agentD ++ "\" is a reserved agent name for a special agent.\nPlease import the agent or choose a different name."
                                      else
                                       do let shape =
                                                        Composite { shapeSegments =
                                                                                 [ Polygon
                                                                                        { shapeStyle = defaultShapeStyle
                                                                                        , shapePerimeter =
                                                                                           [ DoublePoint 0 (-0.7)
                                                                                           , DoublePoint (-0.7) 0.15
                                                                                           , DoublePoint 0.7 0.15
                                                                                           ]
                                                                                        }
                                                                                 , Text { shapeStyle = defaultShapeStyle
                                                                                        , shapeText = symbD} ]}
                                          case (reads :: ReadS [Port] ) portsT of
                                            [(ports,"")] ->
                                                  if haveRepeateds (map fst ports)
                                                        then errorDialog diaW "Bad Ports" "port names repeated"
                                                        else stop $ Just (agentD, shape, ports)
                                            _ -> do { errorDialog diaW "Parse error in list of Ports"                                                     "Parse error in list of Ports"
                                                   -- ; set portsC [ bgcolor := red ]
                                                    }
                                }
                            2 ->  -- Management symbol
                              do{ i <- get symbs3 selection
                                ; let symb = (shapes managementSymbols) !! i
                                      agentD = fst symb
                                      (shape, ports, _) = snd symb
                                ; if agentD `elem` paletteNames
                                     then errorDialog diaW "Repeated symbol name" $ "Already exists one symbol with name \"" ++ agentD ++ "\". Choose a different one."
                                     else
                                        stop $ Just (agentD, shape, ports)
                                }
                            ]
                   set ca   [on command := stop Nothing ]

createHelpWindow :: IO ()
createHelpWindow =
  do f <- frame [ text := "Interaction Nets editor help"
                        , position      := pt 200 20
                        , clientSize    := sz 300 240 ]

     hw <- htmlWindowCreate f 1 (Rect 50 150 500 150) 5 "theWindow"
     htmlWindowLoadPage hw "html/HowToUse.html"
     set f [layout := fill $ widget hw]

createAboutWindow :: Frame () -> IO ()
createAboutWindow f =
    do infoDialog f ("About " ++ toolName) $
          toolName ++ " is an Interaction Nets Editor.\n"
          ++ "The project is mainly developed by\n"
          ++ "Miguel Vilaca < " ++ "jmvilaca@di.uminho.pt" ++" >\n"
          ++ "See the project webpage at\n"
          ++ "http://haskell.di.uminho.pt/jmvilaca/INblobs"



safeAndClear :: Window a -> TextCtrl b -> IO c -> IO ()
safeAndClear theFrame textlog comp =
  safetyNet theFrame $ textCtrlClear textlog >> comp

-- | Add text to a 'TextCtrl' putting it in the given 'Color'.
addTxtOfColor2TextCtrl :: Color -> TextCtrl () -> String -> IO ()
addTxtOfColor2TextCtrl color txt str =
     do
        start <- textCtrlGetInsertionPoint txt
        textCtrlAppendText txt str
        end <- textCtrlGetInsertionPoint txt
        style <- textAttrCreateDefault
        textAttrSetTextColour style color
        textCtrlSetStyle txt start end style
        return ()

addError2TextCtrl, addGood2TextCtrl :: TextCtrl () -> String -> IO ()
addError2TextCtrl = addTxtOfColor2TextCtrl red
addGood2TextCtrl = addTxtOfColor2TextCtrl green

-- | Add text to a 'TextCtrl' putting it in the current position.
addTxtInPlace2TextCtrl :: TextCtrl () -> String -> IO ()
addTxtInPlace2TextCtrl t str =
     do
        pos <- textCtrlGetInsertionPoint t
        textCtrlSetInsertionPoint t pos
        textCtrlWriteText t str

