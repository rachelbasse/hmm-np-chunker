
; counts->hmm.scm output
(define opt-log 'yes)
(define opt-emit 'transition)
(define opt-double-count 'no)
(define opt-smooth-type 'none)
(define opt-smooth-param '())
(make-matrix 'init #(#(-230.25850929940458 -1.0222607040773168 -.44594444648050796)))
(make-matrix 'trans #(#(-3.9854078246035054 -1.1693500120852716 -.3992150877629932) #(-230.25850929940458 -.7520063676980415 -.6375606628132534) #(-3.2589302114540852 -.8493174244966983 -.6276146447985448)))
(make-matrix 'emit #(#(-230.25850929940458 -230.25850929940458 -5.638888334738211 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -7.536008319624092 -7.536008319624092 -2.2178883257798754 -7.248326247172311 -5.338783742287872 -2.8948276961129675 -230.25850929940458 -2.507751424178017 -7.941473427732256 -230.25850929940458 -6.688710459236888 -230.25850929940458 -230.25850929940458 -230.25850929940458 -6.236725335493831 -8.634620608292202 -8.634620608292202 -230.25850929940458 -4.123761101775352 -6.069671250830665 -230.25850929940458 -6.688710459236888 -230.25850929940458 -.876287140801291 -230.25850929940458 -230.25850929940458 -3.830599563558945 -5.267324778305728 -4.039500758157612 -8.634620608292202 -230.25850929940458 -230.25850929940458 -6.069671250830665 -6.555179066612366 -1.5470469027342286 -3.597668005878573 -4.1572837938139955) #(-8.930611771828653 -9.623758952388599 -230.25850929940458 -11.127836349164873 -4.528646353550941 -6.992669792422517 -11.127836349164873 -9.18192620010956 -5.293025612102268 -10.72237124105671 -6.908328643988766 -6.021890875264292 -11.820983529724819 -8.930611771828653 -10.72237124105671 -11.820983529724819 -5.904781467117384 -10.434689168604928 -5.929339317899047 -230.25850929940458 -3.737037827429197 -9.741541988044983 -2.609943402634362 -4.211121328811264 -4.2200810701827365 -8.642929699376873 -11.127836349164873 -3.4968897682744142 -3.4788582663912275 -2.339242917871138 -10.211545617290717 -3.1305095261667755 -2.1830904531764665 -7.530524088576427 -7.310124023207968 -4.919246323068244 -2.740637954364823 -2.8995259507769355 -3.0923955340289195 -2.7694044501335773 -2.998513957455849 -7.041860036613289 -1.4116426950492578 -6.147660262553326) #(-11.538325144602943 -230.25850929940458 -9.235740051608898 -7.997365820565629 -8.38132472345283 -230.25850929940458 -7.577511975005366 -6.432379670702363 -10.439712855934834 -6.09807428216624 -5.136407947875758 -9.053418494814943 -6.614701227496317 -5.666207355127527 -4.180131391869911 -5.460682901253909 -9.053418494814943 -4.033933585441705 -9.095978109233739 -7.913984211626579 -10.845177964042998 -7.13160589733869 -8.299646692438563 -7.596743336933253 -7.3042186400056845 -2.4880361607749872 -3.963766660400463 -6.326110477108318 -9.287033345996448 -8.18842105732834 -7.874763498473297 -5.3388306839908 -6.283437335982243 -1.6095597848825467 -1.9874849433709554 -2.190050586537447 -7.504084506450548 -8.120598460989578 -6.12000498566021 -5.78893215869469 -8.676124263673476 -2.581716405708994 -7.143875989930504 -1.200202106658871)))
(make-matrix 'emit2start #(#(-230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458) #(-6.317390293065218 -6.605072365516999 -230.25850929940458 -230.25850929940458 -5.352309397021631 -230.25850929940458 -230.25850929940458 -230.25850929940458 -4.014805200071172 -230.25850929940458 -230.25850929940458 -7.298219546076944 -230.25850929940458 -7.703684654185108 -230.25850929940458 -230.25850929940458 -8.396831834745054 -230.25850929940458 -4.733270188615407 -230.25850929940458 -6.45092168568974 -8.396831834745054 -5.757774505129795 -6.605072365516999 -1.5844867405675747 -7.010537473625163 -230.25850929940458 -3.353406717825807 -7.010537473625163 -230.25850929940458 -230.25850929940458 -1.8500464239845296 -230.25850929940458 -5.831882477283517 -230.25850929940458 -5.757774505129795 -4.568190438255958 -4.659162216461685 -4.505011536634427 -1.8703369751742636 -5.305789381386738 -6.199607257408834 -1.023457524835005 -6.787393922310953) #(-230.25850929940458 -230.25850929940458 -8.977525200965243 -230.25850929940458 -8.284378020405297 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -6.674940107971196 -6.338467871349983 -5.981792927411251 -5.263953134260934 -4.9344739331306915 -2.3858514689565835 -7.878912912297133 -230.25850929940458 -6.898083659285406 -230.25850929940458 -8.977525200965243 -230.25850929940458 -5.3139635548355955 -230.25850929940458 -230.25850929940458 -8.977525200965243 -3.714835012060356 -4.295393973841022 -7.368087288531141 -230.25850929940458 -230.25850929940458 -230.25850929940458 -8.284378020405297 -230.25850929940458 -1.2060364408476258 -2.674906225220337 -2.7084289172589804 -7.185765731737187 -230.25850929940458 -7.031615051909928 -5.5435379964800955 -8.977525200965243 -1.100507305342844 -6.898083659285406 -2.6603605142179583)))
(make-matrix-3 'emit2 #(#(#(-230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -4.418840607796598 -230.25850929940458 -3.7256934272366524 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -1.123003741792269 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -4.418840607796598 -230.25850929940458 -230.25850929940458 -230.25850929940458 -2.627081138568543 -230.25850929940458 -230.25850929940458 -1.5284688499004333 -2.8094026953624978 -3.0325462466767075 -230.25850929940458 -230.25850929940458 -230.25850929940458 -4.418840607796598 -230.25850929940458 -1.6462518855568167 -230.25850929940458 -3.7256934272366524) #(-230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -5.288988271259519 -230.25850929940458 -230.25850929940458 -7.234898420314831 -6.136286131646722 -230.25850929940458 -6.541751239754887 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -2.128952946414251 -230.25850929940458 -1.4759966464375511 -230.25850929940458 -5.848604059194941 -7.234898420314831 -230.25850929940458 -230.25850929940458 -1.5965437509810858 -2.9864031782654727 -230.25850929940458 -4.595841090699573 -2.702298927161576 -230.25850929940458 -4.932313327320786 -6.136286131646722 -6.136286131646722 -4.749991770526831 -5.848604059194941 -3.2646065067627097 -1.6589493171685155 -7.234898420314831 -3.140553858092731 -4.290459441148391) #(-230.25850929940458 -230.25850929940458 -230.25850929940458 -8.005033344637111 -230.25850929940458 -230.25850929940458 -230.25850929940458 -5.807808767300891 -230.25850929940458 -230.25850929940458 -6.213273875409055 -7.311886164077165 -8.005033344637111 -4.341471698507464 -230.25850929940458 -6.213273875409055 -8.005033344637111 -5.06059436547067 -230.25850929940458 -6.906421055969 -230.25850929940458 -230.25850929940458 -6.906421055969 -230.25850929940458 -5.171820000580894 -3.332204510175204 -230.25850929940458 -4.86953912870796 -230.25850929940458 -230.25850929940458 -230.25850929940458 -6.059123195581797 -8.005033344637111 -2.3079398581317054 -2.113389132811339 -1.9389252545333622 -230.25850929940458 -5.607138071838739 -5.1146615867409455 -5.52012669484911 -6.618738983517219 -4.539297441837384 -6.618738983517219 -.6404863303814681)) #(#(-230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458) #(-8.363692300430476 -230.25850929940458 -230.25850929940458 -8.874517924196468 -6.322471971570838 -5.798742942968939 -8.586835851744686 -7.139916868808361 -4.565958441404458 -8.363692300430476 -4.834394916141005 -7.110929331935108 -9.567665104756411 -6.837635996935427 -230.25850929940458 -9.567665104756411 -7.721838414258081 -8.268382120626152 -8.181370743636522 -230.25850929940458 -5.8622563486912656 -9.567665104756411 -4.380279298915657 -3.1119425696987744 -3.8716908125480227 -6.640925702689373 -8.586835851744686 -2.5069026566490957 -5.566410965600324 -4.067768364810006 -9.973130212864577 -4.19702707662875 -4.092597226463877 -5.662331087479063 -7.530783177495372 -2.820469890786717 -1.5509072590395658 -2.2780545833052446 -2.0825476782080403 -1.944838087798538 -4.628406473502384 -4.955850376049653 -2.535629425263938 -4.27268663947389) #(-230.25850929940458 -230.25850929940458 -7.270182397398733 -10.042771119638514 -230.25850929940458 -230.25850929940458 -6.4874230581491 -5.931897255465203 -7.368622470211985 -4.043834557691831 -4.388529090542449 -8.25101165041046 -4.662873766098055 -10.042771119638514 -2.276778040230839 -7.368622470211985 -230.25850929940458 -3.1025486505188753 -10.042771119638514 -7.368622470211985 -230.25850929940458 -5.242856856857911 -8.53869372286224 -230.25850929940458 -7.963329577958678 -3.6927583842101193 -2.7939114917086023 -6.473238423157144 -10.042771119638514 -230.25850929940458 -10.042771119638514 -230.25850929940458 -230.25850929940458 -2.8901107975606544 -3.6450084781184757 -3.422697913108158 -5.548532494357705 -230.25850929940458 -6.5018117956012 -3.9594113078262763 -7.963329577958678 -.6478617183278974 -4.541512909093787 -3.2337318135955346)) #(#(-230.25850929940458 -230.25850929940458 -5.74597495819864 -230.25850929940458 -230.25850929940458 -230.25850929940458 -230.25850929940458 -6.998737926694008 -7.691885107253953 -2.3096862567252145 -6.439122138758585 -5.440593308647458 -2.982354905941619 -230.25850929940458 -2.6812498131576974 -7.286419999145789 -230.25850929940458 -5.900125638025898 -230.25850929940458 -230.25850929940458 -230.25850929940458 -6.305590746134063 -8.385032287813898 -8.385032287813898 -230.25850929940458 -3.367752450998974 -5.3893000142599075 -230.25850929940458 -6.775594375379798 -230.25850929940458 -.9668514650871104 -230.25850929940458 -230.25850929940458 -3.164676462735574 -4.600842653895637 -3.3036279228294356 -8.385032287813898 -230.25850929940458 -230.25850929940458 -5.340509850090475 -6.593272818585843 -1.6540141873318155 -3.684551922021482 -3.408298545393324) #(-230.25850929940458 -10.070596763884918 -230.25850929940458 -230.25850929940458 -4.060964912821488 -11.169209052553029 -230.25850929940458 -10.070596763884918 -7.31906145084297 -230.25850929940458 -8.03371483662388 -5.8461990734146205 -230.25850929940458 -10.476061871993084 -8.771313779754658 -230.25850929940458 -5.407157669772851 -11.169209052553029 -5.489036443535961 -230.25850929940458 -3.2689430157853274 -8.53015172293777 -2.1816373021859823 -7.091671608647309 -7.277388754442402 -10.476061871993084 -230.25850929940458 -5.970712021287203 -3.0480258104742 -1.8688451779292572 -8.866623959558982 -3.1823641513916456 -1.6702370590894766 -8.604259695091491 -5.954273294944043 -6.185602430844692 -7.1802250059887545 -4.387016996546237 -5.478849598228968 -4.903907839815319 -2.6298630564956578 -8.604259695091491 -1.2720414287617199 -6.878749611404637) #(-9.857160851548088 -230.25850929940458 -230.25850929940458 -7.72892914569882 -6.841625950697917 -230.25850929940458 -10.773451583422244 -8.099302933995714 -11.466598763982189 -230.25850929940458 -7.084572129308307 -7.389061320076469 -10.773451583422244 -5.4676622020355055 -9.674839294754133 -5.182464602911386 -7.4963068504300665 -6.360653290081608 -7.659936274211868 -9.674839294754133 -9.164013670988142 -230.25850929940458 -7.122793342128505 -6.007013249838029 -6.0549527121271485 -2.477778262204118 -9.387157222302353 -5.24402249591082 -6.902250572514352 -6.591401440781037 -6.591401440781037 -3.7550497843530426 -4.693518388326653 -1.67695202786982 -1.823761944846811 -2.236749925617963 -8.694010041742407 -6.662577719248932 -4.902743237450061 -6.730400315587693 -6.695914139516524 -6.704424829184433 -7.000690645327605 -.9571299652736696))))
