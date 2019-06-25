library(xml2)
# library(XML)
library(rvest)
synonymes=xml2::read_xml("fredesc2019.xml")


xml_name(synonymes)
children=xml_children(synonymes)

n=sample(length(children),1)
child=children[[n]]
nm=child%>%html_node("DescriptorName")%>%html_text()
related=child%>%html_nodes("SeeRelatedList")
concept=child%>%html_nodes("ConceptList>Concept>TermList>Term>String")
nm
concept

html_text(child)
as_list(concept)


# check WoNEF https://wonef.fr/data/ et autres http://globalwordnet.org/wordnets-in-the-world/
# check wolf http://pauillac.inria.fr/~sagot/index.html#wolf