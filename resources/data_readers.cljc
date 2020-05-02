{
 long hyperfiddle.readers/long-clj-reader
 dbname hypercrud.types.DbName/dbname-clj-reader
 entity hypercrud.types.ThinEntity/entity-clj-reader
 uri contrib.uri/uri-clj-reader
 ;env contrib.config/env-clj-reader ; no env reads in .clj files
 schema hyperfiddle.readers/schema-clj-reader

 ; defrecords get these for free in clj
 hypercrud.types.DbRef.DbRef hypercrud.types.DbRef/map->DbRef
 hypercrud.types.EntityRequest.EntityRequest hypercrud.types.EntityRequest/map->EntityRequest
 hypercrud.types.Err.Err hypercrud.types.Err/map->Err
 hypercrud.types.QueryRequest.QueryRequest hypercrud.types.QueryRequest/map->QueryRequest}
