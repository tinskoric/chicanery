local function ensureDirectoryExists(path)
    local success, errorMessage = love.filesystem.createDirectory(path)
    if not success then
        error("Failed to create directory: " .. errorMessage)
    end
    return success
end

-- Function to write progress to a loading bar
local function writeProgressBar(progress, maxProgress)
    local barLength = 40
    local progressBar = string.rep("=", math.floor(progress / maxProgress * barLength))
    local remainingBar = string.rep(" ", barLength - #progressBar)
    love.graphics.print(string.format("[%s%s] %.0f%%", progressBar, remainingBar, (progress / maxProgress) * 100), 10, 20)
end

-- Function to scan image and identify regions/provinces/tiles
local function scanImage(imageData)
    local shapeData = {}
    local width, height = imageData:getWidth(), imageData:getHeight()

    for y = 1, height do
        for x = 1, width do
            local r, g, b, a = imageData:getPixel(x - 1, y - 1)

            -- Skip black pixels (empty spaces)
            if r == 0 and g == 0 and b == 0 then
                goto continue
            end

            local color = string.format("%d,%d,%d", r, g, b)
            local shape = shapeData[color]
            if not shape then
                shapeData[color] = { points = {}, centroid = {x = 0, y = 0}, color = color }
                shape = shapeData[color]
            end

            -- Add pixel to shape
            table.insert(shape.points, {x = x, y = y})
            shape.centroid.x = shape.centroid.x + x
            shape.centroid.y = shape.centroid.y + y

            ::continue::
        end
    end

    -- Calculate centroid
    for _, shape in pairs(shapeData) do
        shape.centroid.x = shape.centroid.x / #shape.points
        shape.centroid.y = shape.centroid.y / #shape.points
    end

    return shapeData
end

-- Function to load images using LOVE2D's image loader
local function loadImage(filename)
    return love.image.newImageData("map/" .. filename)
end

-- Function to generate .txt files for regions, provinces, and tiles/seas
local function generateTextFiles(regions, provinces, tilesSeas)
    -- Ensure that the map_data directory exists
    local mapDataDir = "map/map_data"
    ensureDirectoryExists(mapDataDir)

    -- Create and initialize the REGIONS.txt file
    local regionsFile, err = love.filesystem.newFile(mapDataDir .. "/REGIONS.txt", "w")
    if not regionsFile then
        error("Failed to open REGIONS.txt for writing: " .. err)
    end

    -- Create and initialize the PROVINCES.txt file
    local provincesFile, err = love.filesystem.newFile(mapDataDir .. "/PROVINCES.txt", "w")
    if not provincesFile then
        error("Failed to open PROVINCES.txt for writing: " .. err)
    end

    -- Create and initialize the TILES_SEAS.txt file
    local tilesSeasFile, err = love.filesystem.newFile(mapDataDir .. "/TILES_SEAS.txt", "w")
    if not tilesSeasFile then
        error("Failed to open TILES_SEAS.txt for writing: " .. err)
    end

    -- Initialize progress bar
    local progress = 0
    local maxProgress = #regions + #provinces + #tilesSeas
    local progressStep = maxProgress / 100

    -- Write to the REGIONS file
    regionsFile:write("REGION ID, Color, Centroid\n")
    local regionID = 1
    for color, shape in pairs(regions) do
        local regionIDStr = string.format("REGION_%03d", regionID)
        local centroid = shape.centroid
        regionsFile:write(string.format("%s: %s, Centroid: (%.2f, %.2f)\n", regionIDStr, color, centroid.x, centroid.y))
        regionID = regionID + 1
        progress = progress + 1
        writeProgressBar(progress, maxProgress)
    end

    -- Write to the PROVINCES file
    provincesFile:write("PROVINCE ID, Color, Centroid\n")
    local provinceID = 1
    for color, shape in pairs(provinces) do
        local provinceIDStr = string.format("PROVINCE_%03d", provinceID)
        local centroid = shape.centroid
        provincesFile:write(string.format("%s: %s, Centroid: (%.2f, %.2f)\n", provinceIDStr, color, centroid.x, centroid.y))
        provinceID = provinceID + 1
        progress = progress + 1
        writeProgressBar(progress, maxProgress)
    end

    -- Write to the TILES_SEAS file
    tilesSeasFile:write("TILE/SEA ID, Color, Centroid, Impassable\n")
    local tileID = 1
    local seaID = 1
    for color, shape in pairs(tilesSeas) do
        local isSea = (color == "0,0,255")  -- Blue color indicates SEA
        local shapeType = isSea and "SEA" or "TILE"
        local impassableFlag = (color == "255,255,255") and "IMPASSABLE: TRUE" or "IMPASSABLE: FALSE"
        local shapeID = string.format("%s_%03d", shapeType, isSea and seaID or tileID)
        tilesSeasFile:write(string.format("%s: %s, Centroid: (%.2f, %.2f), %s\n", shapeID, color, shape.centroid.x, shape.centroid.y, impassableFlag))

        -- Increment IDs
        if isSea then
            seaID = seaID + 1
        else
            tileID = tileID + 1
        end

        progress = progress + 1
        writeProgressBar(progress, maxProgress)
    end

    -- Close the text files after writing
    regionsFile:close()
    provincesFile:close()
    tilesSeasFile:close()

    print("All text files have been generated and written.")
end

-- LOVE2D callbacks
function love.load()
    -- Load the images using love.image.newImageData
    local regionsImageData = loadImage("REGIONS.png")
    local provincesImageData = loadImage("PROVINCES.png")
    local tilesSeasImageData = loadImage("TILES_SEAS.png")

    -- Scan images for shapes
    regions = scanImage(regionsImageData)
    provinces = scanImage(provincesImageData)
    tilesSeas = scanImage(tilesSeasImageData)

    -- Create and initialize text files for regions, provinces, and tiles/seas
    generateTextFiles(regions, provinces, tilesSeas)
end

-- Render function to display tiles and seas
function love.draw()
    -- Set background color
    love.graphics.setBackgroundColor(1, 1, 1)

    -- Render each tile/sea shape
    love.graphics.setColor(0, 0, 0) -- Black color for shape outline
    for _, shape in pairs(tilesSeas) do
        for _, point in ipairs(shape.points) do
            love.graphics.rectangle("fill", point.x, point.y, 1, 1)
        end
    end
end
