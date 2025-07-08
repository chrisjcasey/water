#' Get or Create WSNZ Database Connection (Singleton)
#'
#' Maintains a single database connection across function calls. If the connection is
#' not yet established or has been closed, a new one is created. Otherwise, the existing
#' connection is returned.
#'
#' @param force Logical. If TRUE, closes existing connection and creates a new one.
#'
#' @return A live `DBI` connection object.
#'
#' @import DBI
#' @import odbc
#' @import stringi
#' @export
GetWSNZConnection<-function(force=FALSE){
  # Use package environment to store the connection object
  if(!exists(".wsnz_env", envir=.GlobalEnv)){
    assign(".wsnz_env", new.env(parent=emptyenv()), envir=.GlobalEnv)
  }
  e = get(".wsnz_env", envir=.GlobalEnv)

  if(!exists("con", envir=e) || force){
    con = NULL
  } else {
    con = e$con
  }

  # Check if it's valid
  if(is.null(con) || !DBI::dbIsValid(con) || force){
    if(!is.null(con)) try(DBI::dbDisconnect(con), silent=TRUE)
    con = DBI::dbConnect(odbc::odbc(),
                         Driver="SQL Server",
                         Server="heimatau.database.windows.net",
                         Database="WSNZ",
                         Port=1433,
                         UID=Sys.getenv("WSNZDBUSER"),
                         PWD=Sys.getenv("WSNZDBPASS"),
                         timeout = 60,
                         ConnectionTimeout = "60")
    e$con = con
  }

  return(e$con)
}

#' Cache Water Safety Data Locally
#'
#' Connects to the WSNZ SQL Server database, fetches the full drowning dataset,
#' performs preprocessing, attaches derived variables, loads GIS shapefiles,
#' and saves the final objects into an `.RData` file for downstream use.
#'
#' @return None. Side effect is a saved file `Data/data.RData` in the working directory.
#' @export
#' @import DBI odbc sqldf sf lubridate dplyr
CacheData <- function()
{
  con = GetWSNZConnection(TRUE)

  df <- tryCatch({
    dbGetQuery(con, "exec GetAllPrevDrowningData")
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    return(NULL)
  })

  if (is.null(df)) return(NULL)

  df$Date = as.Date(df$Date)
  df$DateOfBirth = as.Date(df$DateOfBirth)
  df = df[!is.na(df$Date), ]
  df$Month = as.Date(sprintf("%04d-%02d-01", year(df$Date), month(df$Date)))
  df$Summer= NA
  df$Summer[month(df$Date)<3] = year(df$Date[month(df$Date)<3])-1
  df$Summer[month(df$Date)==12] = year(df$Date[month(df$Date)==12])

  df$SexDesc1 = df$SexDesc
  df$AgeGroup1 = df$AgeGroup
  df$AgeGroup1[df$AgeGroup1 %in% c("65-74","75+")] = "65+"
  df$EthnicityDesc1 = df$EthnicityDesc
  df$EthnicityDesc1[df$EthnicityDesc1 %in% c("Pacific Peoples")]="Pasifika"
  df$ReportSite1 = df$ReportSite
  df$ReportSite1[df$ReportSite1 %in% c("Beach","")]="Beaches"
  df$ReportActivity1 = df$ReportActivity
  df$ReportActivity1[df$ReportActivity1 %in% c("Swimming/Water Play")]="Swimming"
  df$ReportActivity1[df$ActivityDesc %in% c("Commercial Fishing","Unknown")]="Other Non-Recreation"
  df$ReportActivity1[df$ActivityDesc %in% c("River Crossing")]="Other Recreation"

  df$YearJJ = df$Year
  df$YearJJ[month(df$Month)>6] = df$YearJJ[month(df$Month)>6]+1

  df$MonthNum = month(df$Date)
  df$Day = day(df$Date)

  df$RegionDesc1=df$RegionDesc
  df$RegionDesc1[df$RegionDesc1 %in% c("Chathams","Other New Zealand")]="Oceanic & Islands"

  df$TAName = df$LocationDesc
  df$TAName[grepl(" Ward", df$TAName)]="Auckland"
  df$TAName = gsub(" Council","",df$TAName)
  df$TAName[df$TAName %in% c("Auckland City","Franklin District","Manukau City","North Shore City",
                             "Papakura District","Rodney District","Waitakere City")]="Auckland"
  df$TAName[grepl(" Ocean", df$TAName)]="Area Outside Territorial Authority"
  df$TAName[df$TAName=="Hutt City"]="Lower Hutt City"
  df$TAName[df$TAName=="Wanganui District"]="Whanganui District"


  df$BloodAlcoholDesc = "No"
  df$BloodAlcoholDesc[df$AlcoholDesc %in% c("Alcohol Involved", "Alcohol & Drugs Involved")]="Yes"
  df$BloodAlcoholDesc[df$BloodAlcohol>=25]="Yes"

  df$BloodAlcoholDesc[ df$BloodAlcoholDesc == "No" & df$AlcoholDesc=="Unknown" ]="Unknown"

  df$LJ = NA
  df$LJ[df$BuoyancyDesc %in% c("Unknown","Not Applicable","Carried but not Worn","Not Available","Worn - Incorrectly Fitted")]=0
  df$LJ[df$BuoyancyDesc %in% c("Worn - Correctly Fitted")]=1

  df$Lifejacket = NA
  df$Lifejacket = "No Lifejacket"
  df$Lifejacket[df$LF==1] = "Lifejacket"

  #df$OHP = GetHolCounts(con,"Official Holiday Period",0,nrow(df))
  #df$Easter = GetHolCounts(con,"Easter",0,nrow(df))

  df$Over34 = !(df$AgeGroup %in% c("00-04","05-14","15-24","25-34")) | df$Age >34
  df$Under16 =  df$AgeGroup %in% c("00-04","05-14") | df$Age <16

  grps= data.frame()
  grps=rbind(grps, data.frame(Cat = "Region",      Grp = sqldf("SELECT DISTINCT RegionDesc1 FROM df WHERE RegionID <> 12 ORDER BY RegionID")$RegionDesc1))
  grps=rbind(grps, data.frame(Cat = "Age",         Grp = sort(unique(df$AgeGroup))))
  grps=rbind(grps, data.frame(Cat = "Ethnicity",   Grp = sort(unique(df$EthnicityDesc1))))
  grps=rbind(grps, data.frame(Cat = "Sex",         Grp = sort(unique(df$SexDesc))))
  grps=rbind(grps, data.frame(Cat = "Activity",    Grp = sort(unique(df$DReportActivity))))
  grps=rbind(grps, data.frame(Cat = "Environment", Grp = sort(unique(df$DReportSite))))
  grps=rbind(grps, data.frame(Cat = "LifeStage",   Grp = c("Preschool","School-aged","Young Adults","Adults","Older Adults")))
  if("Focus" %in% names(df)) grps=rbind(grps, data.frame(Cat = "Focus",       Grp = sort(unique(df$Focus))))
  grps=sqldf("SELECT * FROM grps WHERE Grp != 'Unknown'")
  grps = grps[!is.na(grps$Grp),]
  dfd=df

  tas      = st_read("c:/Code/Assets/statsnz-territorial-authority-2025-clipped-SHP")
  tas      = st_transform(tas, crs = 4326)
  tas$Name = stringi::stri_trans_general(tas$TA2025_V_1 , "Latin-ASCII")
  tas$Name = gsub(" Territory","", tas$Name)

  regs      = st_read("c:/Code/Assets/statsnzregional-council-2023-clipped-generalised-SHP")
  regs      = st_transform(regs, crs = 4326)
  regs$Name = regs$REGC2023_1
  regs$Name = gsub(" Region","",regs$Name)

  regs$Name[regs$REGC2023_V==8]  = "Manawatu-Wanganui"

  regs = regs %>%
    mutate(Name = ifelse(Name %in% c("Tasman", "Nelson"), "Tasman", Name)) %>%
    group_by(Name) %>%
    summarise(
      REGC2023_V = first(REGC2023_V),
      REGC2023_1 = first(REGC2023_1),
      LAND_AREA_ = sum(LAND_AREA_, na.rm = TRUE),
      AREA_SQ_KM = sum(AREA_SQ_KM, na.rm = TRUE),
      Shape_Leng = sum(Shape_Leng, na.rm = TRUE),  # Optional
      geometry = st_union(geometry),
      .groups = "drop"
    )



  roads  = st_read("c:/Code/Assets/statsnz-nz-road-centrelines-topo-150k-SHP")
  roads  = st_transform(roads, crs = 4326)


  # Sample data frame with latitude, longitude, and other fields
  ptsdata = data.frame(
    did = dfd$DrowningID,
    yyr = dfd$Year,
    tau = gsub(" Ward","",gsub(" Council","",dfd$LocationDesc)),
    reg = dfd$RegionDesc1,
    act = dfd$ActivityDesc,
    sit = dfd$SiteDesc,
    age = dfd$AgeGroup1,
    eth = dfd$EthnicityDesc1,
    sex = dfd$SexDesc,
    lat = dfd$Latitude,
    lon = dfd$Longitude,
    fat = dfd$NumberofFatalities,
    loc = dfd$Location
  )

  ptsdata = ptsdata[!is.na(ptsdata$lat),]

  # Convert to sf object
  pts = st_as_sf(ptsdata, coords = c("lon", "lat"), crs = 4326)


  n=nrow(pts)
  r=rep(NA,n)
  i=1;
  while (i <= n)
  {
    if(pts$fat[i]==1)
    {
      r[i]=1; i=i+1
    }
    else
    {
      for(j in 1:pts$fat[i])
        r[i+j-1]=j
      i=i+pts$fat[i]
    }
  }
  pts$ii=r

  dn=data.frame(matrix(c(1980,146,3237742,
                         1981,143,3250794,
                         1982,140,3269674,
                         1983,126,3305201,
                         1984,122,3344766,
                         1985,163,3374754,
                         1986,117,3386906,
                         1987,108,3401909,
                         1988,127,3434981,
                         1989,116,3442220,
                         1990,132,3471211,
                         1991,98,3516000,
                         1992,95,3552200,
                         1993,109,3597800,
                         1994,101,3648300,
                         1995,109,3706700,
                         1996,111,3762300,
                         1997,98,3802700,
                         1998,115,3829200,
                         1999,92,3851100,
                         2000,99,3873100,
                         2001,91,3916200,
                         2002,112,3989500,
                         2003,95,4061600,
                         2004,89,4114300,
                         2005,83,4161000,
                         2006,70,4209100,
                         2007,88,4245700,
                         2008,94,4280300,
                         2009,88,4332100,
                         2010,65,4373900,
                         2011,91,4399400,
                         2012,75,4425900,
                         2013,77,4477400,
                         2014,70,4564400,
                         2015,85,4663700,
                         2016,84,4767600,
                         2017,87,4859500,
                         2018,66,4941200,
                         2019,79,5040400,
                         2020,82,5103700,
                         2021,89,5116500,
                         2022,95,5160600,
                         2023,90,5314300,
                         2024,72,5338500),ncol=3,byrow=T))
  names(dn)=c("year","n","pop")

  pop = data.frame(
    Year = rep(1990:2024, times = 9),
    AgeGroup = rep(c("00-04", "05-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"), each = 35),
    Population = c(
      # "00-04" values
      235326,246986,249601,252428,255820,259565,539342,267197,269600,271014,272639,276188,279737,283285,286834,290383,293932,297481,301030,307200,314350,317460,315730,311930,308810,305740,305020,305980,306380,311027,315674,320321,324968,329615,334263,
      # "05-14" values
      464719,487746,492909,498491,505190,512587,1065087,527659,532403,535195,538404,545413,552421,559429,566437,573445,580453,587461,594470,593900,593750,593240,594070,596870,602290,608560,616580,627720,638220,647900,657581,667261,676942,686622,696303,
      # "15-24" values
      473553,497018,502279,507968,514794,522331,1085334,537689,542524,545369,548639,555780,562921,570063,577204,584345,591487,598628,605770,609370,616890,621780,623500,627400,639230,655460,667180,672340,672420,682619,692818,703017,713217,723416,733615,
      # "25-34" values
      422505,443440,448135,453210,459300,466025,968337,479728,484041,486579,489497,495869,502240,508612,514983,521355,527726,534098,540470,541130,542790,543000,542840,548580,568880,599300,637210,675940,710770,721550,732331,743112,753893,764674,775455,
      # "35-44" values
      491916,516290,521756,527665,534755,542585,1127419,558539,563561,566516,569913,577332,584750,592168,599586,607005,614423,621841,629260,624220,619280,610990,601820,592930,586660,583030,580530,582320,588780,597710,606641,615571,624502,633433,642363,
      # "45-54" values
      466916,490051,495239,500848,507578,515010,1070121,530153,534920,537724,540949,547991,555032,562073,569114,576156,583197,590238,597280,608260,616220,620240,622370,625660,628700,631780,635250,637170,637360,647027,656694,666362,676029,685697,695364,
      # "55-64" values
      356793,374471,378436,382722,387865,393543,817731,405115,408758,410901,413365,418746,424126,429507,434887,440268,445648,451029,456410,470140,483870,497210,504680,512770,524770,537450,553020,569350,584610,593477,602344,611212,620079,628946,637814,
      # "65-74" values
      227337,238601,241127,243858,247135,250753,521032,258126,260447,261813,263383,266811,270240,273668,277096,280525,283953,287381,290810,300440,311510,323860,342200,359800,376430,390940,403320,416320,432490,439050,445610,452170,458730,465290,471850,
      # "75+" values
      190931,200392,202513,204807,207559,210598,437594,216790,218739,219886,221205,224084,226964,229843,232722,235602,238481,241360,244240,247900,251980,256200,260810,266180,273980,283420,295080,306730,314360,319128,323896,328664,333432,338201,342969
    ),
    stringsAsFactors = FALSE
  )

  movsum <- function(x, n)
  {
    res =rep(NA, length(x))
    for(i in n:length(x))  res[i] =sum(x[(i-n+1):i])

    res
  }

  dn$R10=movsum(dn$n,10)/movsum(dn$pop,10)*1e5
  dn$R5=movsum(dn$n,5)/movsum(dn$pop,5)*1e5
  dn$R1=dn$n/dn$pop*1e5

  LOI = read.csv(file.path("c:/Code/Assets/LOI.csv"), encoding = "UTF-8")

  geoms = dbGetQuery(con, "
    SELECT * FROM Geometry g
    JOIN GeometryPoints p ON g.GeometryID = p.GeometryID
    WHERE Labels = 'BLACKSPOT'
    ORDER BY p.GeometryID, PointID
  ")
  save(file="c:/Code/Assets/Volatile/data.RData",dfd,grps,pts,tas,regs,roads,dn,LOI,geoms,pop)
}

utils::globalVariables(c("Name", "REGC2023_V", "REGC2023_1", "LAND_AREA_","AREA_SQ_KM", "Shape_Leng", "geometry"))
