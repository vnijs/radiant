library(stringr)
setwd("~/gh/radiant/R/")
fl <- list.files(pattern = "\\.R$")
fl <- fl[-which(fl %in% c("aaa.R","zzz.R"))]

f <- "single_prop.R"

for(f in fl) {

	source(f)
	fun <- f %>% str_replace(".R","")

	# args <- formals(fun)

	args <- c(formals(fun), formals(paste0("summary.",fun)))
	          # formals(paste0("plot.",fun)))

	# args %<>% c(formals(paste0("predict.",fun)), formals("plot.reg_predict"))

	pre <- str_extract(names(args),"^[a-z]{1,3}_") %>% na.omit %>% unique
	# pre <- str_extract(names(args),"^[a-z]{1,3}_") %>% na.omit %>% unique
	pre <- pre[str_extract(f,"^[a-z]{1}") == str_extract(pre,"^[a-z]{1}")]
	if(length(pre) > 1) {
		print(f)
		print(pre)
		stop("Too many pre's")
	}
	args %<>% names %>% .[str_detect(.,pre)]
	args

	if (length(args) == 0) {
		next
	} else {
		cat("\n\n========================================\n")
		cat("Pre for", f, "is", pre, "\n")
	}

	# args <- args[-which(args == "reg_test_var")]
	# args <- args[-which(args == "glm_check")]
	args

	org <- readLines(f)
	for(a in args) {
  	str <- str_replace(a, pre, "")
	  # str_extract(org,paste0("[^ca_a-zA-Z0-9_]",str)) %>%
	  str_extract(org,paste0("[^a-zA-Z0-9_]",str,"[^a-zA-Z0-9_]")) %>%
	    .[!is.na(.)] %>%
	    { if (length(.) > 0) {
	    	  cat("Working on:",a,"\n")
	    	  print(.)
	    	  stop("Fix arguments here")
	    	} else {
	    	  cat("Fixing",a,"\n")
  				str_replace(org, a, str) %>% cat(file = f, sep="\n")
					org <- readLines(f)
	    	}
	    }
	}



}

		# str_replace(a,pre,"") %>%
		#   {str_detect(org,paste0("[^",pre,"][^a-z]",.))} %>%
		#   any
		#   ?`&`


		# org <- readLines(f)
		# org <- c("ca_dep_var","ca_indep_var","	dep_var")
  # 	str_replace(a,pre,"") %>%
	 #    {str_detect(org,paste0("[^ca_]",.)) &
	 #     str_detect(org,paste0("[^a-zA-Z0-9]",.))} %>%
	 #     { if (any(.))
	 #         for (o in org[.])

	 #     }

	 #     any


		# # org <- c("ca_dep_var","ca_indep_var","ca_dep_var	dep_var ,dep_var", "ca_dep_var ca_indep_var")
		# org <- readLines(f)
  # 	str <- str_replace(a,pre,"")
	 #  str_extract(org,paste0("[^ca_a-zA-Z0-9_]",str)) %>% .[!is.na(.)]


	 #    %>%
	 #    str_replace("[(,\\t\\s]","")

	 #    str_replace(\\[\\(\\{\\,\\.)])

	 #    {str_extract(org,paste0("[^",pre,"]",.))}

	 #    {str_detect(org,paste0("[^a-zA-Z0-9]",.))}

	 #    {str_detect(org,paste0("[^",pre,"]",.))}

		#   & str_detect(org,paste0("[^a-z]",a))} %>%
		#   {str_detect(org,paste0("[^",pre,"]",.)) & str_detect(org,paste0("[^a-z]",a))} %>%
		#   org[.]

