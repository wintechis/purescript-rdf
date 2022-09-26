module RDF.Prefixes where

newtype Prefix = Prefix String

dc :: Prefix
dc = Prefix "http://purl.org/dc/elements/1.1/"

dcterms :: Prefix
dcterms = Prefix "http://purl.org/dc/terms/"

foaf :: Prefix
foaf = Prefix "http://xmlns.com/foaf/0.1/"

geo :: Prefix
geo = Prefix "http://www.opengis.net/ont/geosparql#"

geonames :: Prefix
geonames = Prefix "http://www.geonames.org/ontology#"

ldp :: Prefix
ldp = Prefix "http://www.w3.org/ns/ldp#"

org :: Prefix
org = Prefix "http://www.w3.org/ns/org#"

owl :: Prefix
owl = Prefix "http://www.w3.org/2002/07/owl#"

rdf :: Prefix
rdf = Prefix "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

rdfs :: Prefix
rdfs = Prefix "http://www.w3.org/2000/01/rdf-schema#"

skos :: Prefix
skos = Prefix "http://www.w3.org/2004/02/skos/core#"

vcard :: Prefix
vcard = Prefix "http://www.w3.org/2006/vcard/ns#"

xsd :: Prefix
xsd = Prefix "http://www.w3.org/2001/XMLSchema#"