module BlindtextModule (blindtext1, blindtext1Naturalism, cryptotext1, cryptotext2) where
import NaturalLanguageModule  (naturalismDefault)

import NormalizeLanguageModule (normalizeLanguage)
import SnModule (makePeriodicS_n)
import MascModule (Direction (..), MascKey, masc)


blindtext1 = "Human rights are moral principles that set out certain standards of human behaviour, and are regularly protected as legal rights in national and international law.[1] They are \"commonly understood as inalienable fundamental rights to which a person is inherently entitled simply because she or he is a human being.\"[2] Human rights are thus conceived as universal (applicable everywhere) and egalitarian (the same for everyone). The doctrine of human rights has been highly influential within international law, global and regional institutions. Policies of states and in the activities of non-governmental organizations and have become a cornerstone of public policy around the world. The idea of human rights[3] suggests, \"if the public discourse of peacetime global society can be said to have a common moral language, it is that of human rights.\" The strong claims made by the doctrine of human rights continue to provoke considerable skepticism and debates about the content, nature and justifications of human rights to this day. Indeed, the question of what is meant by a \"right\" is itself controversial and the subject of continued philosophical debate.[4] Many of the basic ideas that animated the human rights movement developed in the aftermath of the Second World War and the atrocities of The Holocaust, culminating in the adoption of the Universal Declaration of Human Rights in Paris by the United Nations General Assembly in 1948. The ancient world did not possess the concept of universal human rights.[5] The true forerunner of human rights discourse was the concept of natural rights which appeared as part of the medieval Natural law tradition that became prominent during the Enlightenment with such philosophers as John Locke, Francis Hutcheson, and Jean-Jacques Burlamaqui, and featured prominently in the English Bill of Rights and the political discourse"

blindtext1Norm = "HUMANRIGHTSAREMORALPRINCIPLESTHATSETOUTCERTAINSTANDARDSOFHUMANBEHAVIOURANDAREREGULARLYPROTECTEDASLEGALRIGHTSINNATIONALANDINTERNATIONALLAWTHEYARECOMMONLYUNDERSTOODASINALIENABLEFUNDAMENTALRIGHTSTOWHICHAPERSONISINHERENTLYENTITLEDSIMPLYBECAUSESHEORHEISAHUMANBEINGHUMANRIGHTSARETHUSCONCEIVEDASUNIVERSALAPPLICABLEEVERYWHEREANDEGALITARIANTHESAMEFOREVERYONETHEDOCTRINEOFHUMANRIGHTSHASBEENHIGHLYINFLUENTIALWITHININTERNATIONALLAWGLOBALANDREGIONALINSTITUTIONSPOLICIESOFSTATESANDINTHEACTIVITIESOFNONGOVERNMENTALORGANIZATIONSANDHAVEBECOMEACORNERSTONEOFPUBLICPOLICYAROUNDTHEWORLDTHEIDEAOFHUMANRIGHTSSUGGESTSIFTHEPUBLICDISCOURSEOFPEACETIMEGLOBALSOCIETYCANBESAIDTOHAVEACOMMONMORALLANGUAGEITISTHATOFHUMANRIGHTSTHESTRONGCLAIMSMADEBYTHEDOCTRINEOFHUMANRIGHTSCONTINUETOPROVOKECONSIDERABLESKEPTICISMANDDEBATESABOUTTHECONTENTNATUREANDJUSTIFICATIONSOFHUMANRIGHTSTOTHISDAYINDEEDTHEQUESTIONOFWHATISMEANTBYARIGHTISITSELFCONTROVERSIALANDTHESUBJECTOFCONTINUEDPHILOSOPHICALDEBATEMANYOFTHEBASICIDEASTHATANIMATEDTHEHUMANRIGHTSMOVEMENTDEVELOPEDINTHEAFTERMATHOFTHESECONDWORLDWARANDTHEATROCITIESOFTHEHOLOCAUSTCULMINATINGINTHEADOPTIONOFTHEUNIVERSALDECLARATIONOFHUMANRIGHTSINPARISBYTHEUNITEDNATIONSGENERALASSEMBLYINTHEANCIENTWORLDDIDNOTPOSSESSTHECONCEPTOFUNIVERSALHUMANRIGHTSTHETRUEFORERUNNEROFHUMANRIGHTSDISCOURSEWASTHECONCEPTOFNATURALRIGHTSWHICHAPPEAREDASPARTOFTHEMEDIEVALNATURALLAWTRADITIONTHATBECAMEPROMINENTDURINGTHEENLIGHTENMENTWITHSUCHPHILOSOPHERSASJOHNLOCKEFRANCISHUTCHESONANDJEANJACQUESBURLAMAQUIANDFEATUREDPROMINENTLYINTHEENGLISHBILLOFRIGHTSANDTHEPOLITICALDISCOURSE"

-- DANGER this of course is cheating and can not be used in a real usecase
blindtext1Naturalism = naturalismDefault blindtext1

-- mutation
---[3,2,1]
cryptotext1 = makePeriodicS_n [10,9..1] . normalizeLanguage $ blindtext1

-- masc
cryptotext2 = masc Encrypt "ZYXWVUTSRQPONMLKJIHGFEDCBA" . normalizeLanguage $ blindtext1

main = do
	print "Naturalism (default) of Cleartext:"
	print . naturalismDefault $ blindtext1
	print "Naturalism (default) of Kryptotext1:"
	print . naturalismDefault $ cryptotext1
	print "Naturalism (default) of Kryptotext2:"
	print . naturalismDefault $ cryptotext2
