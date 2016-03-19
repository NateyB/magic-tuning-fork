toConvert = [[
]]

alt = {

}

converted = string.gsub(toConvert:lower(), ".-\n", function(a)
                if (a:match("%?")) then
                    return ""
                end
                classification, capShape, capSurface, capColor, bruises, odor, gillAttachment, gillSpacing, gillSize, gillColor, stalkShape, stalkRoot, stalkSurfaceAboveRing, stalkSurfaceBelowRing, stalkColorAboveRing, stalkColorBelowRing, veilType, veilColor, ringNumber, ringType, sporePrintColor, population, habitat = string.match(a, string.rep("(%a),", 22) .. "(%a)")
                return string.format("(%s (capShape %s) (capSurface %s) (capColor %s) (bruises %s) (odor %s) (gillAttachment %s) (gillSpacing %s) (gillSize %s) (gillColor %s) (stalkShape %s) (stalkRoot %s) (stalkSurfaceAboveRing %s) (stalkSurfaceBelowRing %s) (stalkColorAboveRing %s) (stalkColorBelowRing %s) (veilType %s) (veilColor %s) (ringNumber %s) (ringType %s) (sporePrintColor %s) (population %s) (habitat %s))\n", (classification == "e" and "+") or "-", capShape, capSurface, capColor, bruises, odor, gillAttachment, gillSpacing, gillSize, gillColor, stalkShape, stalkRoot, stalkSurfaceAboveRing, stalkSurfaceBelowRing, stalkColorAboveRing, stalkColorBelowRing, veilType, veilColor, ringNumber, ringType, sporePrintColor, population, habitat)
            end)

print ("(\n" .. converted .. ")")