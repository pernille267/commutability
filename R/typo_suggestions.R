#' Suggestions for typos
#'
#' @return A list containing the word book for relevant words
#' @export
#'
#' @examples typo_suggestions()

typo_suggestions <- function(){

  typos_replicateid_extra_letter <- c("replicateifd", "replicateicd" ,"replicateivd", "replicateisd",
                                      "replicateiwd", "replicateied", "replicateird", "replicateixd",
                                      "replicateoid", "replicatekid", "replicatelid", "replicateuid",
                                      "replicatejid", "replicatreid", "replicatdeid", "replicatfeid",
                                      "replicatweid", "replicatseid", "replicayteid", "replicagteid",
                                      "replicahteid", "replicarteid", "replicafteid", "replicsateid",
                                      "repliczateid", "replicxateid", "replicqateid", "replicwateid",
                                      "replivcateid", "replixcateid", "repliscateid", "replidcateid",
                                      "replifcateid", "reploicateid", "replkicateid", "repllicateid",
                                      "repluicateid", "repljicateid", "repklicateid", "repilicateid",
                                      "repolicateid", "repplicateid", "reoplicateid", "relplicateid",
                                      "rreplicateid", "rdeplicateid", "rfeplicateid", "rweplicateid",
                                      "rseplicateid", "treplicateid", "freplicateid", "greplicateid",
                                      "ereplicateid", "dreplicateid", "replicateid")

  typos_replicateid_wrong_letter <- c("replicateif", "replicateic", "replicateiv", "replicateis",
                                      "replicateiw", "replicateie", "replicateir", "replicateix",
                                      "replocateid", "replkcateid", "repllcateid", "replucateid",
                                      "repljcateid", "rrplicateid", "rdplicateid", "rfplicateid",
                                      "rwplicateid", "rsplicateid", "replicayeid", "replicageid",
                                      "replicareid", "replicaheid", "replicafeid", "replicsteid",
                                      "repliczteid", "replicxteid", "replicqteid", "replicwteid",
                                      "replivateid", "replixateid", "replisateid", "replidateid",
                                      "replifateid", "replocateid", "replkcateid", "repllcateid",
                                      "replucateid", "repljcateid", "repkicateid", "repiicateid",
                                      "repoicateid", "reppicateid", "reolicateid", "rellicateid",
                                      "rrplicateid", "rdplicateid", "rfplicateid", "rwplicateid",
                                      "rsplicateid", "teplicateid", "feplicateid", "geplicateid",
                                      "eeplicateid", "deplicateid")

  typos_replicateid_missing_letter <- c("replicatei", "replcateid", "rplicateid", "replicaeid",
                                        "replicteid", "repliateid", "replcateid", "repicateid",
                                        "relicateid", "rplicateid", "eplicateid")

  typos_replicateid_double_letter <- c("replicateidd", "replicateiid", "replicateeid", "replicatteid",
                                      "replicaateid", "repliccateid", "repliicateid", "repllicateid",
                                      "repplicateid", "reeplicateid", "rreplicateid")

  typos_replicateid_reverse_letter <- c("replicatedi", "replicatied", "replicaetid", "replictaeid",
                                        "repliacteid", "replciateid", "repilcateid", "relpicateid",
                                        "rpelicateid", "erplicateid")

  typos_replicate_extra_letter <- c("replicatre", "replicatde", "replicatfe", "replicatwe", "replicatse",
                                    "replicayte", "replicagte", "replicahte", "replicarte", "replicafte",
                                    "replicsate", "repliczate", "replicxate", "replicqate", "replicwate",
                                    "replivcate", "replixcate", "repliscate", "replidcate", "replifcate",
                                    "reploicate", "replkicate", "repllicate", "repluicate", "repljicate",
                                    "repklicate", "repilicate", "repolicate", "repplicate", "reoplicate",
                                    "relplicate", "rreplicate", "rdeplicate", "rfeplicate", "rweplicate",
                                    "rseplicate", "treplicate", "freplicate", "greplicate", "ereplicate",
                                    "dreplicate")

  typos_replicate_wrong_letter <- c("rrplicate", "rdplicate", "rfplicate", "rwplicate",
                                    "rsplicate", "replicaye", "replicage", "replicahe",
                                    "replicare", "replicafe", "replicste", "repliczte",
                                    "replicxte", "replicqte", "replicwte", "replivate",
                                    "replixate", "replisate", "replidate", "replifate",
                                    "replocate", "replkcate", "repllcate", "replucate",
                                    "repljcate", "repkicate", "repiicate", "repoicate",
                                    "reppicate", "reolicate", "rellicate", "rrplicate",
                                    "rdplicate", "rfplicate", "rwplicate", "rsplicate",
                                    "teplicate", "feplicate", "geplicate",
                                    "eeplicate", "deplicate")

  typos_replicate_missing_letter <- c("rplicate", "replicae", "replicte", "repliate",
                                      "replcate", "repicate", "relicate",
                                      "rplicate", "eplicate")

  typos_replicate_double_letter <- c("replicatee", "replicatte", "replicaate", "repliccate",
                           "repliicate", "repllicate", "repplicate",
                           "reeplicate", "rreplicate")

  typos_replicate_reverse_letter <- c("replicaet", "replictae", "repliacte", "replciate",
                                      "repilcate", "relpicate", "rpelicate", "erplicate")

  other_typos_replicateid <- c("rep", "repl", "repo", "replic", "replica", "replika",
                               "replikat", "replikk", "replikar", "rplikat", "rplicat",
                               "repeat", "repeatid", "rpeatid", "r", "repid", "rpid", "rid",
                               "repli", "repliid", "rrplicate", "erplicate", "repkicate",
                               "fepeat", "tepeat", "eepeat", "replicab", "replicable", "rekp",
                               "repea", "repeatedid")


  typos_replicateid <- c(typos_replicateid_extra_letter, typos_replicateid_wrong_letter,
                         typos_replicateid_missing_letter, typos_replicateid_double_letter,
                         typos_replicateid_reverse_letter, typos_replicate_wrong_letter,
                         typos_replicate_missing_letter, typos_replicate_double_letter,
                         typos_replicate_reverse_letter, other_typos_replicateid)


  typos_sampleid_extra_letter <- c("sampleifd", "sampleicd", "sampleivd", "sampleisd", "sampleiwd",
                                   "sampleied", "sampleird", "sampleixd", "sampleoid", "samplekid",
                                   "samplelid", "sampleuid", "samplejid", "samplreid", "sampldeid",
                                   "samplfeid", "samplweid", "samplseid", "sampkleid", "sampileid",
                                   "sampoleid", "samppleid", "samopleid", "sanmpleid", "sahmpleid",
                                   "sajmpleid", "sakmpleid", "ssampleid", "szampleid", "sxampleid",
                                   "sqampleid", "swampleid", "dsampleid", "xsampleid", "csampleid",
                                   "asampleid", "qsampleid", "wsampleid", "esampleid", "zsampleid",
                                   "sampleid")

  typos_sampleid_wrong_letter <- c("sampleif", "sampleic", "sampleiv", "sampleis", "sampleiw",
                                   "sampleie", "sampleir", "sampleix", "sampleod", "samplekd",
                                   "sampleld", "sampleud", "samplejd", "samplrid", "sampldid",
                                   "samplfid", "samplwid", "samplsid", "sampkeid", "sampieid",
                                   "sampoeid", "samppeid", "samoleid", "samlleid", "sanpleid",
                                   "sahpleid", "sajpleid","sakpleid", "ssmpleid", "szmpleid",
                                   "sxmpleid", "sqmpleid", "swmpleid", "dampleid", "xampleid",
                                   "campleid", "aampleid", "qampleid", "wampleid", "eampleid",
                                   "zampleid")

  typos_sampleid_missing_letter <- c("samplei", "sampled", "samplid", "sampeid",
                                     "samleid", "sapleid", "smpleid", "ampleid")

  typos_sampleid_double_letter <- c("sampleidd", "sampleiid", "sampleeid", "samplleid",
                                    "samppleid", "sammpleid", "saampleid", "ssampleid")

  typos_sampleid_reverse_letter <- c("sampledi", "samplied", "sampelid", "samlpeid",
                                     "sapmleid", "smapleid", "asmpleid")

  typos_sample_extra_letter <- c("samplre", "samplde", "samplfe", "samplwe", "samplse",
                                 "sampkle", "sampile", "sampole", "sampple", "samople",
                                 "samlple", "sanmple", "sahmple", "sajmple", "sakmple",
                                 "ssample", "szample", "sxample", "sqample", "swample",
                                 "dsample", "xsample", "csample", "asample", "qsample",
                                 "wsample", "esample", "zsample")

  typos_sample_wrong_letter <- c("samplr", "sampld", "samplf", "samplw", "sampls", "sampke",
                                 "sampie", "sampoe", "samppe", "samole", "samlle", "sanple",
                                 "sahple", "sajple", "sakple", "ssmple", "szmple", "sxmple",
                                 "sqmple", "swmple", "dample", "xample", "cample", "aample",
                                 "qample", "wample", "eample", "zample")

  typos_sample_missing_letter <- c("sampl", "sampe", "samle", "saple", "smple", "ample")

  typos_sample_double_letter <- c("samplee", "samplle", "sampple", "sammple", "saample", "ssample")

  typos_sample_reverse_letter <- c("sampel", "samlpe", "sapmle", "smaple", "asmple")

  other_typos_sampleid <- c("subject", "subjectid", "subid", "subjid", "sid", "pid",
                            "dubject", "wubject", "sibject", "sybject", "suvject", "sunject",
                            "dubjectid", "wubjectid", "sibjectid", "sybjectid", "suvjectid", "sunjectid",
                            "patient", "pat", "pati", "patien", "patuent", "parient", "pstient",
                            "cs", "csid", "csi", "cid", "ps", "psam", "psample", "psampl", "psam",
                            "subjud", "smpl", "sampla", "patienta", "pasient", "pasientid",
                            "pasiid", "prove", "prov", "proveid", "provid", "prid")

  typos_sampleid <- c(typos_sampleid_extra_letter, typos_sampleid_wrong_letter,
                      typos_sampleid_missing_letter, typos_sampleid_double_letter,
                      typos_sampleid_reverse_letter, typos_sample_extra_letter,
                      typos_sample_wrong_letter, typos_sample_missing_letter,
                      typos_sample_double_letter, typos_sample_reverse_letter,
                      other_typos_sampleid)

  return(list("SampleID" = typos_sampleid, "ReplicateID" = typos_replicateid))

}




