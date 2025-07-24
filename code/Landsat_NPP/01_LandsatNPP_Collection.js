/*******************************************************************************
 * Introduction *
 * 
 *  1) Collect the annual Landsat Net Primary Production within
 *     the base plots of all NEON sites (v11).
 * 
 * Last updated: 7/24/2025
 * 
 * Runtime: <1m ~ 2m
 * 
 * Author: Chenyang Wei (chenyangwei.cwei@gmail.com)
 ******************************************************************************/


/*******************************************************************************
 * Modules *
 ******************************************************************************/

var IMG_mod = require(
  "users/ChenyangWei/Public:Modules/General/Image_Analysis&Processing.js");

var VIS_mod = require(
  "users/ChenyangWei/Public:Modules/General/Visualization.js");


/*******************************************************************************
 * Objects *
 ******************************************************************************/

// Projection information.
var prj_30m = {
  crs: "EPSG:4326",
  scale: 30
};

// Major working directories.
var wd_Main_1_Str = "projects/invasive-mefa/assets/NEON_NPP/";


/*******************************************************************************
 * Functions *
 ******************************************************************************/

// Re-project the Landsat NPP.
function Reproject_NPP(LandsatNPP_Img) {
  LandsatNPP_Img = LandsatNPP_Img.setDefaultProjection(prj_30m);
  
  return LandsatNPP_Img;
}


/*******************************************************************************
 * Datasets *
 ******************************************************************************/

// NEON plots.
var NEONplots_FC = ee.FeatureCollection(
  wd_Main_1_Str 
  + "NEON_Plots/"
  + "All_NEON_TOS_Plot_Polygons_V11"
);

// Annual Landsat NPP.
var LandsatNPP_IC = ee.ImageCollection("UMT/NTSG/v2/LANDSAT/NPP")
  .select("annualNPP");

// // NEON canopy height model.
// var NEONchm = ee.ImageCollection(
//   "projects/neon-prod-earthengine/assets/CHM/001"
// );


/*******************************************************************************
 * 1) Pre-process the NEON plots and the Landsat NPP data. *
 ******************************************************************************/

// Select the base plots.
NEONplots_FC = NEONplots_FC.filter(
  ee.Filter.eq({
    name: "subtype", 
    value: "basePlot"
  })
);

// Select the columns of interest.
NEONplots_FC = NEONplots_FC.select([
  "longitude",
  "latitude",
  "elevation",
  "site.*",
  "plot.*",
  "pointID",
  "subtype"
]);

// Extract the Landsat NPP during the study period.
var startYear_Num = 2013;

var endYear_Num = 2020;

LandsatNPP_IC = LandsatNPP_IC.filter(
  ee.Filter.calendarRange({
    start: startYear_Num, 
    end: endYear_Num, 
    field: "year"
  })
);

// Reproject the NPP data.
LandsatNPP_IC = LandsatNPP_IC.map(Reproject_NPP);

// Convert the annual NPP ImageCollection to a multi-band Image.
var LandsatNPP_Img = ee.Image();

for (var year_Num = startYear_Num; year_Num <= endYear_Num; year_Num ++) {
  
  // Extract the annual NPP.
  var annualNPP_Img = LandsatNPP_IC.filter(
    ee.Filter.calendarRange({
      start: year_Num, 
      field: "year"
    })
  ).first().rename("NPP_" + year_Num);
  
  // Add a mask band.
  var annualNPP_Mask_Img = annualNPP_Img.mask()
    .rename("NPP_m_" + year_Num);
  
  LandsatNPP_Img = LandsatNPP_Img
    .addBands(annualNPP_Img)
    .addBands(annualNPP_Mask_Img);
}

LandsatNPP_Img = LandsatNPP_Img.select("NPP_.*");


/*******************************************************************************
 * 2) Average the Landsat NPP within each NEON base plot. *
 ******************************************************************************/

var avgNPP_NEONplots_FC = LandsatNPP_Img.reduceRegions({
  collection: NEONplots_FC, 
  reducer: ee.Reducer.mean(), 
  scale: prj_30m.scale, 
  crs: prj_30m.crs, 
  maxPixelsPerRegion: 1e13
});


/*******************************************************************************
 * Results *
 ******************************************************************************/

// Whether to export the result(s).
var export_Bool = true; // true OR false.

if (!export_Bool) {
  
  /****** Check the dataset(s) and object(s). ******/
  
  print("NEONplots_FC:",
    NEONplots_FC.first(),
    NEONplots_FC.size()); // 2442.
  
  print("avgNPP_NEONplots_FC:",
    avgNPP_NEONplots_FC.first(),
    avgNPP_NEONplots_FC.size()); // 2442.
  
  IMG_mod.Print_ImgInfo(
    "LandsatNPP_IC.first():", 
    LandsatNPP_IC.first()
  );
  
  IMG_mod.Print_ImgInfo(
    "LandsatNPP_Img:", 
    LandsatNPP_Img
  );
  
  var plotID_Example_Str = "HARV_025";
  
  var plotExample_FC = NEONplots_FC.filter(
    ee.Filter.eq({
      name: "plotID", 
      value: plotID_Example_Str
    })
  );
  
  // Visualization.
  Map.setOptions("Satellite");
  Map.centerObject(plotExample_FC, 16);
  
  Map.addLayer(LandsatNPP_Img.select("NPP_2017"), 
    {
      min: 0.0,
      max: 10000.0,
      palette: VIS_mod.NDVI_palette
    }, 
    "NPP_2017");

  Map.addLayer(LandsatNPP_Img.select("NPP_2018"), 
    {
      min: 0.0,
      max: 10000.0,
      palette: VIS_mod.NDVI_palette
    }, 
    "NPP_2018");

  Map.addLayer(LandsatNPP_Img.select("NPP_m_2017"), 
    {
      min: 0,
      max: 1,
      palette: "00FFFF, FF0000"
    }, 
    "NPP_m_2017");

  Map.addLayer(LandsatNPP_Img.select("NPP_m_2018"), 
    {
      min: 0,
      max: 1,
      palette: "00FFFF, FF0000"
    }, 
    "NPP_m_2018");

  Map.addLayer(NEONplots_FC, 
    {
      color: "FFFFFF"
    }, 
    "NEONplots_FC");
  
  Map.addLayer(plotExample_FC, 
    {
      color: "FF0000"
    }, 
    "plotExample_FC");

} else {
  
  var outputName_Str = "WtdAvg_AnnualNPP_2013to2020_AllBasePlots";
  
  // Export to Asset and Drive.
  Export.table.toAsset({
    collection: avgNPP_NEONplots_FC, 
    description: outputName_Str, 
    assetId: wd_Main_1_Str 
      + "LandsatNPP/"
      + outputName_Str
  });
  
  Export.table.toDrive({
    collection: avgNPP_NEONplots_FC, 
    description: outputName_Str, 
    folder: outputName_Str, 
    fileFormat: "SHP"
  });
  
}

