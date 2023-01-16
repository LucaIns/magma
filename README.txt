Source code to process the data and replicate the results in:
	
	Insolia, L., Guerrier, S., Montagna, C. P., Victoria-Feser, M. P., & Caricchi, L. (2022). 
	"Estimation and Uncertainty Quantification of Magma Interaction Times using Statistical Emulation".


The main folder includes the following R scripts.

	- pre_processing_quantiles.R: data preprocessing.
	It samples from the full/lower half/bottom half of the shallow reservoir and computes the quantiles of interest.
	
	- code_matching_graph.R: statistical emulation for the models described in Figure 4.

	- code_matching_graph_subsamples_up_low.R: statistical emulation for the models described in Figure 5.

	- sensitivity_sim.R: sensistivity analysis of the emulator's output described in Figure 6.

	- plots_generation.R: generates Figures 4-6.


The "plots" sub-folder includes the resulting plots.

The "data" sub-folder includes processed data.