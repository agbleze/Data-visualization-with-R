## creating sf geometries
library(sf)
st_point(c(4, 3))

st_point(c(5, 3, 2))

st_point(c(5, 2, 1), dim = "XYM")
st_point(c(5,2,3,1))

# use matrices for multiline
# Use rbind  create matrices
multiplepoint <- rbind(c(5, 2), c(1, 3), c(5,6), c(1, 3))
st_multipoint(multiplepoint)

## use matrice for linestring
st_linestring(multiplepoint)

## use list for polygon 
polygon_list = list(rbind(c(1, 5), c(2, 2), c(2,4), c(5, 3), c(2, 3), c(1, 5)))
st_polygon(polygon_list)

## polygon with a hole
polygon_border = rbind(c(1, 5), c(2,2), c(4,1), c(4,4), c(1,5))
polygon_hole = rbind(c(2,4), c(3,4), c(3,3), c(2,3), c(2,4))
polygon_with_hole = list(polygon_border, polygon_hole)
st_polygon(polygon_with_hole)

## multilinestring
multilinestring_list = list(rbind(c(1,5), c(4,4), c(4,1), c(2,2), c(3,2)),
                            rbind(c(1,2), c(2,4)))
st_multilinestring((multilinestring_list))

## multipolygon
multipolygon_list = list(list(rbind(c(1,5),c(2,2), c(4,1), c(4,4), c(1,5))),
                         list(rbind(c(0,2), c(1,2), c(1,3), c(0,3), c(0,2))))
st_multipolygon(multipolygon_list)

## geometrycollection
gemetrycollection_list = list(st_multipoint(multiplepoint),
                              st_linestring(multiplepoint))
st_geometrycollection(gemetrycollection_list)


### Simple feature column sfc
#### sfc is a list of sfg objects 
####sfc represent the geometry column in sf
# using sfc point
point1 = st_point(c(1,2), c(7,1))
point2 = st_point(c(3,4), c(1,2))
points_sfc = st_sfc(point1, point2)
points_sfc
st_geometry_type(points_sfc)

### sfc polygon
polygon_list1 = list(rbind(c(1, 5), c(2,2), c(4,1), c(4,4), c(1,5)))
polygon1 = st_polygon(polygon_list1)
polygon_list2 = list(rbind(c(0,2), c(2,4), c(3,4), c(1,2), c(0,2)))
polygon2 = st_polygon(polygon_list2)
polygon_sfc = st_sfc(polygon1, polygon2)
st_geometry_type(polygon_sfc)
st_crs(polygon_sfc)

### EPSG definition
points_sfc_wgs= st_sfc(point1, point2, crs = 4326)
st_crs(points_sfc_wgs)

## adding proj4string
polygon_wgs = st_sfc(polygon1, polygon2, crs = "+proj=longlat +datum=WGS84 +no_defs")
st_crs(polygon_wgs)

#### adding attribute to simple features
## 1. create sfg object of points
condPoint <- st_point(c(21.2, -12.8))
## 2. create sfc object with crs
condGeom = st_sfc(condPoint, crs = 4326)
## 3. create a dataframe for attribute data
condAttri <- data.frame(
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
condsf = st_sf(condAttri, geometry = condGeom)
condsf
class(condsf)
