## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, results='asis'----------------------------------------------
DiagrammeR::grViz("digraph {
  graph [layout = dot, rankdir = LR]
  
  node [shape = circle, fixedsize=TRUE, fontsize=7]        
  rec1 [label = <Host<BR/>seeking>]
  rec2 [label = <Host<BR/>found>]
  rec3 [label = <Fed>]
  rec4 [label = <Resting>]
  rec5 [label = <Ovipositing>]
  
  # edge definitions with the node IDs
  rec1 -> rec2 [label= <P<SUB>Ai</SUB><BR/> >,fontsize=7]
  rec2 -> rec3 [label= <P<SUB>Bi</SUB><BR/> >,fontsize=7]
  rec3 -> rec4 [label= <P<SUB>Ci</SUB><BR/> >,fontsize=7]
  rec4 -> rec5 [label= <P<SUB>Di</SUB><BR/> >,fontsize=7]
  rec5 -> rec1 [label= <P<SUB>Ei</SUB><BR/> >,fontsize=7]
  }",
  width = 700)

## ---- echo=FALSE, results='asis'----------------------------------------------
DiagrammeR::grViz("digraph {
  graph [layout = dot, rankdir = LR]
  
  node [shape = rectangle]        
  rec1 [label = <Define entomological parameters <BR/><B>def_vector_params()</B>>]
  rec2 [label = <Define mosquito and human activity patterns <BR/><B>def_activity_patterns()</B>>]
  rec3 [label = <Define human host parameters <BR/><B>def_host_params()</B>>]
  rec4 [label = <Parameterize entomological model <BR/><B>build_model_obj()</B>>]
  rec5 [label = <Define interventions effects <BR/><B>def_interventions_effects()</B>>]
  rec6 [label = <Calculate impact on vectorial capacity <BR/><B>calculate_impact()</B>>]
  
  # edge definitions with the node IDs
  rec1 -> rec4 
  rec2 -> rec4 
  rec3 -> rec4
  rec4 -> rec6
  rec5 -> rec6
  }",
  width = 720)

## ---- dpi=150, fig.cap="Output figure generated with the standard call of the AnophelesModel() function"----
# load the AnophelesModel package
library(AnophelesModel)

# call main function with default values
results_gambiae = AnophelesModel()

## -----------------------------------------------------------------------------
# Print the object
host_ent_param

## -----------------------------------------------------------------------------
# print the first entries of the vec_ent_param object
head(vec_ent_param)

## -----------------------------------------------------------------------------
vec_ent_param[vec_ent_param$species_name == "Anopheles albimanus", ]

## -----------------------------------------------------------------------------
select_idx = activity_patterns$species == "Homo sapiens" &
                                  activity_patterns$country == "Kenya"
Kenya_human_rhythms = activity_patterns[select_idx,]
head(Kenya_human_rhythms)

## ---- dpi=150, fig.cap="Example of human activity patterns in Rachuonyo, Kenya. The proportion of humans indoors (labeled IND) out of the total human population is displayed."----
library(ggplot2)
Rachuonyo_human_rhythms = Kenya_human_rhythms[which(Kenya_human_rhythms$id == 2),]
labels = substr(unique(Rachuonyo_human_rhythms$hour), 1, 5)
Rachuonyo_human_rhythms$hour = factor(labels, levels = labels )
ggplot(Rachuonyo_human_rhythms, aes(x=hour, y=value, group=site)) +
    theme_light() + theme_linedraw() + theme_bw(base_size = 5) +
    theme(legend.position = "bottom") +
    geom_line(size=0.5) + 
    scale_x_discrete(name = "Time of the day (hh.mm)")+ 
    guides(color = guide_legend(nrow = 2,byrow = TRUE)) +
    labs(title = "Proportion of humans indoors in Rachuonyo, Kenya", fill="",
         x="Time", y="Activity")

## -----------------------------------------------------------------------------
select_idx = activity_patterns$species == "Anopheles gambiae" &
                                  activity_patterns$country == "Kenya"
Kenya_gambiae_biting = activity_patterns[select_idx,]
head(Kenya_gambiae_biting)

## -----------------------------------------------------------------------------
print(interventions_param$interventions_summary[which(interventions_param$interventions_summary$Intervention == "LLINs"),])

## -----------------------------------------------------------------------------
# load the necesary packages
library(reshape)

DuraNet = get_net_decay(net_type = "DuraNet", country = "Kenya", insecticide_type = "DuraNet", n_ips = 100, duration = 3)

## ---- fig.show='hold'---------------------------------------------------------
library(ggpubr)
# Plot the survival
ggplot(DuraNet, aes(x=time, y=survival)) + geom_point() +
    theme_light() + theme_linedraw() + theme_bw() + ggtitle("Survival of the net")

# Plot the log of the holed area
ggplot(DuraNet, aes(x=time, y=logHoles)) + geom_point() +
    theme_light() + theme_linedraw() + theme_bw() + ggtitle("Log holed area")

# Plot the insecticide content
ggplot(DuraNet, aes(x=time, y=insecticideContent)) + geom_point() +
    theme_light() + theme_linedraw() + theme_bw() + ggtitle("Insecticide decay")

## -----------------------------------------------------------------------------
nili_ent_params = def_vector_params(mosquito_species = "Anopheles nili")
head(nili_ent_params)

## -----------------------------------------------------------------------------
# Use own parameterisation
species_name = "Anopheles example"
M = 0.623
M.sd = 0
Chi = 0.939
A0 = 0.313
A0.sd = 0
zeta.3 = 1
td = 0.33
tau = 3
ts = 10
to = 5
endophily = 1
endophily.sd = 0
endophagy = 1
endophagy.sd = 0
custom_ent_params_table = as.data.frame(cbind.data.frame(species_name, M, M.sd, Chi,
                                A0, A0.sd, zeta.3, td,
                                tau, ts, to, endophily, endophily.sd, endophagy, endophagy.sd))

## -----------------------------------------------------------------------------
custom_ent_params = def_vector_params(mosquito_species = species_name, vector_table = custom_ent_params_table)
head(custom_ent_params)

## -----------------------------------------------------------------------------
default_host_params = def_host_params()
print(default_host_params)

## -----------------------------------------------------------------------------
species_name = "Anopheles example" 
host = c("human", "animal") 
PBi = c(0.67, 0.44)
PCi = c(0.33, 0.434)
PDi = c(0.647, 0.344)
PEi = c(0.672, 0.214)
Kvi = c(0.52, 0.314)
custom_params_tab = cbind.data.frame(species_name, host, PBi, PCi, PDi, PEi, Kvi)

## -----------------------------------------------------------------------------
custom_host_params = def_host_params(mosquito_species = species_name, host_table = custom_params_tab)
print(custom_host_params)

## -----------------------------------------------------------------------------
activity_p = def_activity_patterns(activity = "default_Anopheles_gambiae")
print(activity_p)

## -----------------------------------------------------------------------------
activity_list = NULL
activity_list$HBI = 34
activity_list$HBO = 165
activity_list$humans_indoors = 4
activity_list$humans_in_bed = 24
activity_p2 = def_activity_patterns(activity_list)
print(activity_p2)

## -----------------------------------------------------------------------------
# Briet et al 2019, Table S5. Biting rhythm of An. albimanus in Haiti, Dame Marie
HBI = c(0.22, 0.21, 0.22, 0.10, 0.13, 0.17, 0.08, 0.12, 0.03, 0.12, 0.17, 0.18, 0.25)
HBO = c(0.25, 0.35, 0.37, 0.33, 0.36, 0.32, 0.13, 0.14, 0.09, 0.15, 0.36, 0.23, 0.25)
# Human activity in Haiti from Briet et al 2019, Knutson et al. 2014
humans_in_bed = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964, 83.202297, 92.736838, 96.734455, 96.637604, 92.983136, 85.264515, 73.021653, 57.079495)/100 
humans_indoors = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964, 83.202297, 92.736838, 96.734455, 96.637604, 92.983136, 85.264515, 73.021653, 57.079495)/100 
custom_params = as.data.frame(cbind(HBI, HBO, humans_indoors,humans_in_bed ))
custom_activity_obj = def_activity_patterns(custom_params)
print(custom_activity_obj)

## -----------------------------------------------------------------------------
# Define size of the host and mosquito population 
host_pop = 2000
vec_pop = 10000

## -----------------------------------------------------------------------------
model_params = build_model_obj(custom_ent_params, default_host_params, custom_activity_obj, host_pop)
# Print the calculated mosquito death rate
print(model_params$host_params$muvA)
# Print the estimated availability to mosquitoes for each host type
print(model_params$host_params$alphai)

## -----------------------------------------------------------------------------
print(intervention_obj_examples)

## -----------------------------------------------------------------------------
new_IRS = intervention_obj_examples$IRS_example
new_IRS$description = "Permethrin IRS"
new_IRS$parameterisation = "IRS16"

## -----------------------------------------------------------------------------
new_intervention_list = c(intervention_obj_examples, list(new_IRS = new_IRS))

## -----------------------------------------------------------------------------
list_interv = new_intervention_list
coverages = c(seq(0, 1, by = 0.1))
n_ip = 100
# Calculate the intervention effects:
intervention_vec = def_interventions_effects(list_interv, model_params, n_ip)

## -----------------------------------------------------------------------------
in_out_exp = get_in_out_exp(activity_cycles = custom_activity_obj, vec_p = custom_ent_params)
print(in_out_exp)

## ---- dpi=150, fig.cap="Output figure with the impact on the vectorial capacity for different species"----
impacts = calculate_impact(intervention_vec, coverages, model_params,
vec_pop, n_ip)

## ---- dpi=150, fig.cap="Impact of interventions on vectorial capacity."-------
p = plot_impact_species(impacts, "VC_red")
plot(p)

## ---- dpi=150, fig.cap="Impact of interventions on vectorial capacity including the variability in the entomological parameters specific to the mosquito species."----
impact_gambiae = calculate_impact_var(mosquito_species = "Anopheles gambiae",
                                        activity_patterns = "default_Anopheles_gambiae",
                                        interventions = intervention_vec,
                                        n_sample_points = 10,
                                        plot_result = FALSE)
plot_impact_var("Anopheles gambiae", impact_gambiae)

## -----------------------------------------------------------------------------
entomology_xml = get_OM_ento_snippet(nili_ent_params, default_host_params)
print(entomology_xml)

## ---- dpi=150-----------------------------------------------------------------

GVI_snippets = get_OM_GVI_snippet("Anopheles example", impacts$interventions_vec$LLINs_example,
                    100, GVI_file = "", file_append = FALSE, plot_f = TRUE)

