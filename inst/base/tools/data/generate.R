# regression, glm_reg, and conjoint all need data.frames as input.

# use commands such as the ones below to generate data_frames for input.

# make a list with the levels for each variable in the selected data and min/max
# for numeric variables. From there use expand.grid to make a table.

# in conjoint you may want to add check-boxes to select profiles to use in
# calculating market/preference shares

# make a reactiveValues object called rp_data (or a list in r_data?) that will hold
# the prediction data - need to be able to find them both in and outside of
# radiant

# this might also be really useful for simulation but then you need to allow simulating
# from distributions ...

# pred_df <- try(eval(parse(text = paste0("with(result$model$model, expand.grid(", glm_predict_cmd ,"))"))), silent = TRUE)
