module RenderSVG(renderSVG, exportSVG) where
import Graphics.Gloss
import Graphics.Svg
import qualified Data.Text as T
import Data.Maybe
import Text.Printf

-- Generates least and greatest co-ordinates to create a bounding box
picExtremities :: Picture -> Maybe ((Int, Int), (Int, Int))
picExtremities Blank = Nothing
picExtremities (Color _ pic) = picExtremities pic
picExtremities (Line path)
    | not (null path) = Just ((minX, minY), (maxX, maxY))
    | otherwise = Nothing
        where
            xCoords = map (round . fst) path
            yCoords = map (round . snd) path
            (minX, minY) = (minimum xCoords, minimum yCoords)
            (maxX, maxY) = (maximum xCoords, maximum yCoords)
picExtremities (Pictures pics)
    | not (null extremities) = Just ((minX, minY), (maxX, maxY))
    | otherwise = Nothing
        where
            extremities = catMaybes $ map picExtremities pics
            (minXs, minYs) = unzip $ map (fst) extremities
            (maxXs, maxYs) = unzip $ map (snd) extremities
            (minX, minY) = (minimum minXs, minimum minYs)
            (maxX, maxY) = (maximum maxXs, maximum maxYs)
picExtremities (Bitmap _) = Just ((0, 0), (60, 60))
-- Let's just pretend everything is a square...
picExtremities (Rotate _angle pic) = picExtremities pic
picExtremities (Translate x y pic) =
    case picExtremities pic of
        Just ((minX, minY), (maxX, maxY)) ->
            Just ((minX + (round x), minY + (round y)),
                  (maxX + (round x), maxY + (round y)))
        Nothing -> Nothing
picExtremities _ = undefined

picOffsets :: Picture -> (Int, Int)
picOffsets pic =
    case picExtremities pic of
        Just ((minX, minY), (maxX, maxY)) ->
           ((minX + maxX) `div` 2, (minY + maxY) `div` 2)
        Nothing -> (0, 0)

renderSVG :: (Int, Int) -> Picture -> Element
renderSVG (w, h) glossPic =
    doctype <> with (svg11_ svgPic)
        [Version_ <<- T.pack "1.1", Width_ <<- showT w, Height_ <<- showT h]
    where
        (offsetX, offsetY) = picOffsets glossPic
        svgPic =
            g_ [Transform_ <<- trans] (renderSVG' black glossPic)
        --
        trans :: T.Text
        trans = T.pack $ printf "translate(%d %d)" ((div w 2) - offsetX) ((div h 2) + offsetY)
        --
        showT :: Show a => a -> T.Text
        showT = T.pack . show
        --
        showF :: Float -> T.Text
        showF = showT . round
        --
        renderSVG' _ Blank = mempty
        renderSVG' col (Line path) = renderPath col path
        renderSVG' _ (Color col pic) = renderSVG' col pic
        renderSVG' col (Pictures pics) = mconcat $ map (renderSVG' col) pics
        -- Note: this renders the bitmap as the turtle
        renderSVG' _ (Bitmap _) =
            image_ [Width_ <<- showT 60, Height_ <<- showT 60, XlinkHref_ <<- T.pack "turtle.png"]
        renderSVG' col (Translate x y pic) =
            let xOffset = ((round x) :: Int) - 30 in
            let yOffset = (-((round y) :: Int) - 30) in
            g_ [Transform_ <<- T.pack (printf "translate(%d %d)" xOffset yOffset)] (renderSVG' col pic)
        renderSVG' col (Rotate angle pic) =
            g_ [Transform_ <<- T.pack (printf "rotate(%s 30 30)" (showF angle))] (renderSVG' col pic)
        renderSVG' _ nope = error $ "SVG export undefined for Picture: " ++ (show nope)
        --
        showColor :: Color -> T.Text
        showColor col = T.pack $ printf "rgba(%d, %d, %d, %d)" qr qg qb qa
            where
                q :: Float -> Int
                q = round . ((*) 255.0)
                (qr, qg, qb, qa) = (q r, q g, q b, q a)
                (r, g, b, a) = rgbaOfColor col

        --
        renderPath :: Color -> Path -> Element
        renderPath _ [] = mempty
        renderPath _ [_] = mempty -- Single point: no line
        renderPath col ((x0, y0) : (x1, y1) : xs) =
            line_
                [X1_ <<- showF x0,
                 Y1_ <<- showF (-y0),
                 X2_ <<- showF x1,
                 Y2_ <<- showF (-y1),
                 Stroke_ <<- showColor col]
            <> renderPath col ((x1, y1) : xs)

exportSVG :: (Int, Int) -> FilePath -> Picture -> IO ()
exportSVG size path pic = writeFile path (show (renderSVG size pic))
