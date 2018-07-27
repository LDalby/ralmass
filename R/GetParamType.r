#' Add type to input parameter string
#'
#' Add the input parameter type to the string of just the parameter name.
#'
#' @param param character The input parameter
#' @return character param plus the type added in parentheses
#' @export
GetParamType <- function(param) {
  if (missing(param)) {
    stop("Input argument missing \n")
  }
  param = stringr::str_trim(param, side = "both")
  switch(
    EXPR = param,
    # Names of the paramters:
    "GOOSE_MINFORAGEOPENNESS" = "GOOSE_MINFORAGEOPENNESS (float)",
    "GOOSE_AFTERDARKTIME" = "GOOSE_AFTERDARKTIME (int)",
    "GOOSE_FOLLOWINGLIKELYHOOD_BN" = "GOOSE_FOLLOWINGLIKELYHOOD_BN (int)",
    "GOOSE_FOLLOWINGLIKELYHOOD_PF" = "GOOSE_FOLLOWINGLIKELYHOOD_PF (int)",
    "GOOSE_FOLLOWINGLIKELYHOOD_GL" = "GOOSE_FOLLOWINGLIKELYHOOD_GL (int)",
    "GOOSE_MAXAPPETITESCALER" = "GOOSE_MAXAPPETITESCALER (float)",
    "GOOSE_MAXENERGYRESERVEPROPORTION" = "GOOSE_MAXENERGYRESERVEPROPORTION (float)",
    "GOOSE_INITIALENERGYRESERVEPROPORTION" = "GOOSE_INITIALENERGYRESERVEPROPORTION (float)",
    "GOOSE_LEAVINGTHRESHOLD" = "GOOSE_LEAVINGTHRESHOLD (float)",
    "GOOSE_FORAGEDIST_BN" = "GOOSE_FORAGEDIST_BN (float)",
    "GOOSE_FORAGEDIST_PF" = "GOOSE_FORAGEDIST_PF (float)",
    "GOOSE_FORAGEDIST_GL" = "GOOSE_FORAGEDIST_GL (float)",
    "GOOSE_MINFORAGEDECAYRATE" = "GOOSE_MINFORAGEDECAYRATE (float)",
    "GOOSE_ROOSTLEAVINGLIKELYHOOD" = "GOOSE_ROOSTLEAVINGLIKELYHOOD (int)",
    "GOOSE_MEM_MINMEMVALUE" = "GOOSE_MEM_MINMEMVALUE (int)",
    "GOOSE_GRAINDECAYRATEWINTER" = "GOOSE_GRAINDECAYRATEWINTER (float)",
    "GOOSE_GRAINDECAYRATESPRING" = "GOOSE_GRAINDECAYRATESPRING (float)",
    "HUNTERS_DISTRIBUTE_RULESET" = "HUNTERS_DISTRIBUTE_RULESET (int)",
    "GOOSE_ROOSTLEAVEDISTSD" = "GOOSE_ROOSTLEAVEDISTSD (int)",
    "GOOSE_ROOSTLEAVEDISTMEAN" = "GOOSE_ROOSTLEAVEDISTMEAN (int)",
    "GOOSE_MEM_EXPECTEDFORAGINGTIME" = "GOOSE_MEM_EXPECTEDFORAGINGTIME (int)",
    "GOOSE_MODELEXITDAY" = "GOOSE_MODELEXITDAY (int)",
    "MAP_WEATHER_FILE" = "MAP_WEATHER_FILE (string)",
    "FARMSIZEPROBPARAMONE" = "FARMSIZEPROBPARAMONE (float)",
    "ROOSTDISTPROBPARAMONE" = "ROOSTDISTPROBPARAMONE (float)",
    "GOOSE_BN_STARTNOS" = "GOOSE_BN_STARTNOS (int)",
    "GOOSE_BN_SPRING_MIG_NOS" = "GOOSE_BN_SPRING_MIG_NOS (int)",
    "GOOSE_BN_ARRIVEDATESTART" = "GOOSE_BN_ARRIVEDATESTART (int)",
    "GOOSE_BN_ARRIVEDATEEND" = "GOOSE_BN_ARRIVEDATEEND (int)",
    "GOOSE_PF_STARTNOS" = "GOOSE_PF_STARTNOS (int)",
    "GOOSE_PF_SPRING_MIG_NOS" = "GOOSE_PF_SPRING_MIG_NOS (int)",
    "GOOSE_BN_STARTNOS" = "GOOSE_BN_STARTNOS (int)",
    "GOOSE_PF_STARTNOS" = "GOOSE_PF_STARTNOS (int)",
    "GOOSE_GL_STARTNOS" = "GOOSE_GL_STARTNOS (int)",
    "GOOSE_GL_SPRING_MIG_NOS" = "GOOSE_GL_SPRING_MIG_NOS (int)",
    "GOOSE_GL_STARTNOS" = "GOOSE_GL_STARTNOS (int)",
    "GOOSE_OPENSEASON_END" = "GOOSE_OPENSEASON_END (int)",
    "GOOSE_PF_OPENSEASONEND" = "GOOSE_PF_OPENSEASONEND (int)",
    "GOOSE_RUNTIMEREPORTING" = "GOOSE_RUNTIMEREPORTING (bool)",
    "GOOSE_FLIGHTCOST" = "GOOSE_FLIGHTCOST (float)",
    "GOOSE_PINKFOOTWEIGHT" = "GOOSE_PINKFOOTWEIGHT (float)",
    "GOOSE_BARNACLEWEIGHT" = "GOOSE_BARNACLEWEIGHT (float)",
    "GOOSE_GREYLAGWEIGHT" = "GOOSE_GREYLAGWEIGHT (float)",
    "GOOSE_METABOLICCONVCOSTS" = "GOOSE_METABOLICCONVCOSTS (float)",
    "GOOSE_TIMEDCOUNTS" = "GOOSE_TIMEDCOUNTS (int)",
    "GOOSE_FIELDFORAGEDIST_PF" = "GOOSE_FIELDFORAGEDIST_PF (float)",
    "GOOSE_FIELDFORAGEDIST_BN" = "GOOSE_FIELDFORAGEDIST_BN (float)",
    "GOOSE_FIELDFORAGEDIST_GL" = "GOOSE_FIELDFORAGEDIST_GL (float)",
    "GOOSE_BMRCONSTANTA" = "GOOSE_BMRCONSTANTA (float)",
    "GOOSE_BMRCONSTANTB" = "GOOSE_BMRCONSTANTB (float)",
    "GOOSE_THERMALCONSTANTA_PF" = "GOOSE_THERMALCONSTANTA_PF (float)",
    "GOOSE_THERMALCONSTANTA_BN" = "GOOSE_THERMALCONSTANTA_BN (float)",
    "GOOSE_THERMALCONSTANTA_GL" = "GOOSE_THERMALCONSTANTA_GL (float)",
    "GOOSE_THERMALCONSTANTB" = "GOOSE_THERMALCONSTANTB (float)",
    "GOOSE_ENERGYCONTENTOFFAT" = "GOOSE_ENERGYCONTENTOFFAT (float)",
    "GOOSE_PF_YOUNG_PROPORTION" = "GOOSE_PF_YOUNG_PROPORTION (float)",
    "GOOSE_BN_YOUNG_PROPORTION" = "GOOSE_BN_YOUNG_PROPORTION (float)",
    "GOOSE_GL_YOUNG_PROPORTION" = "GOOSE_GL_YOUNG_PROPORTION (float)",
    "GOOSE_DAYTIMEBMRMULTIPLIER" = "GOOSE_DAYTIMEBMRMULTIPLIER (float)",
    "GOOSE_NIGHTTIMEBMRMULTIPLIER" = "GOOSE_NIGHTTIMEBMRMULTIPLIER (float)",
    "GOOSE_ROOSTCHANGECHANCE" = "GOOSE_ROOSTCHANGECHANCE (float)",
    "GOOSE_DIST_WEIGHTING_POWER" = "GOOSE_DIST_WEIGHTING_POWER (float)",
    "GOOSE_SNOW_SCALER" = "GOOSE_SNOW_SCALER (float)",
    "GOOSE_GRASS_TO_WINTER_CEREAL_SCALER" = "GOOSE_GRASS_TO_WINTER_CEREAL_SCALER (float)",
    "GOOSE_HUNTER_HUNT_LENGTH" = "GOOSE_HUNTER_HUNT_LENGTH (int)",
    "HUNTERS_MAXDENSITY" = "HUNTERS_MAXDENSITY (float)",
    "HUNTERS_RECORDBAG" = "HUNTERS_RECORDBAG (bool)",
    "HUNTERS_DISTRIBUTE" = "HUNTERS_DISTRIBUTE (bool)",
    "HUNTER_REFRACTIONPERIOD" = "HUNTER_REFRACTIONPERIOD (int)",
    "HUNTER_MAGAZINECAPACITY" = "HUNTER_MAGAZINECAPACITY (int)",
    "HUNTER_HUNTDAYPROBSCALER" = "HUNTER_HUNTDAYPROBSCALER (float)",
    "HUNTER_LARGEFIELDGOOSEPROXIMITYCHANCE" = "HUNTER_LARGEFIELDGOOSEPROXIMITYCHANCE (float)",
    "HUNTER_LARGEFIELDGOOSEPROXIMITYCHANCESIZECUTOFF" = "HUNTER_LARGEFIELDGOOSEPROXIMITYCHANCESIZECUTOFF (int)",
    "HUNTER_CHECK_FREQUENCY" = "HUNTER_CHECK_FREQUENCY (float)",
    "HUNTER_PINKFOOTBAGLIMIT" = "HUNTER_PINKFOOTBAGLIMIT (int)",
    "HUNTER_GREYLAGBAGLIMIT" = "HUNTER_GREYLAGBAGLIMIT (int)",
    "CLOSESTFARMPROBPARAMONE" = "CLOSESTFARMPROBPARAMONE (float)",
    # These are a special case for the hunter model and are supposed to
    # stay unchanged. This a hack so GenerateParams still works.
    "HuntingDays" = "HuntingDays",
    "WeekdayHunterChance" = "WeekdayHunterChance",
    "GooseLookChance" = "GooseLookChance",
    "Efficiency" = "Efficiency",
    # Default:
    "MissingParameter"
  )
}
