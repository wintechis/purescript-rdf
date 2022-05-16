module RDF.Prefixes where

newtype Prefix = Prefix String

arena :: Prefix
arena = Prefix "https://solid.ti.rw.fau.de/public/ns/arena#"

ldp :: Prefix
ldp = Prefix "http://www.w3.org/ns/ldp#"

ra :: Prefix
ra = Prefix "https://solid.ti.rw.fau.de/public/ns/robotArm#"

rdf :: Prefix
rdf = Prefix "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

rdfs :: Prefix
rdfs = Prefix "http://www.w3.org/2000/01/rdf-schema#"

xsd :: Prefix
xsd = Prefix "http://www.w3.org/2001/XMLSchema#"