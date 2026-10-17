#================================================================================
  #Portfolio Project: Two-Way Analysis of Variance (ANOVA) with Interactions
#Concepts: Factorial ANOVA, Interaction Effects, Diagnostic Plots, Post-Hoc Tests
      #Dataset: built-in 'warpbreaks' (The breaks in Yarn during weaving)
#================================================================================

#Load the built-in system dataset into the local environment
data("warpbreaks")


                           #1.Model Estimation
# ------------------------------------------------------------------------------
   #Fit a Two-Way AVONA model by the main effect and the interaction effect
    #Explanatory variables(wool * tension) on the response variable(breaks)

two.way.Wool <- aov(breaks~wool*tension, data = warpbreaks)



                    #2.STATISTICAL ASSUMPTION DIAGNOSTICS
# ------------------------------------------------------------------------------
#Create a Normal Q-Q Plot of the residuals to assess normality
plot(two.way.Wool, 2)


#Create a Residuals vs Fitted plot to evaluate homoscedasticity (equal variance)
plot(two.way.Wool, 1)



                      #3.ANALYSIS OF VARIANCE SUMMARY
# ------------------------------------------------------------------------------
#Print the ANOVA table to evaluate F-statistics and p-values for main effects
#(wool, tension) and the joint interaction term (wool:tension).
summary(two.way.Wool)


                       #4.INTERACTION VISUALIZATIONS
# ------------------------------------------------------------------------------
# Plot interaction layouts to visualize how changes in tension impact yarn 
# breakage across different types of wool fibers.

#Interaction layout 1: Grouped primarily by Wool type
interaction.plot(
  x.factor     = warpbreaks$wool, 
  trace.factor = warpbreaks$tension, 
  response     = warpbreaks$breaks,
  main         = "Interaction Plot: Tension Effects across Wool Categories",
  xlab         = "Wool Type",
  ylab         = "Mean Number of Breaks",
  col          = c("blue", "pink", "orange")
)

#Interaction layout 2: Grouped primarily by Tension level
interaction.plot(
  x.factor     = warpbreaks$tension, 
  trace.factor = warpbreaks$wool, 
  response     = warpbreaks$breaks,
  main         = "Interaction Plot: Wool Performance across Tension Tiers",
  xlab         = "Tension Level",
  ylab         = "Mean Number of Breaks",
  col          = c("green", "red"))


                      #5.POST-HOC PAIRWISE COMPARISONS
# ------------------------------------------------------------------------------
#Executing Tukey Honestly Significant Difference test strictly isolating tension
TukeyHSD(two.way.Wool, "tension")


