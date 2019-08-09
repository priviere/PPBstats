#' Get ggplot to visualize output from \code{\link{mean_comparisons.check_model_bh_intra_location}}
#'
#' @description
#' \code{plot.mean_comparisons_model_bh_intra_location} returns ggplot to visualize outputs from \code{\link{mean_comparisons.check_model_bh_intra_location}}
#'
#' @param x Output from \code{\link{mean_comparisons.check_model_bh_intra_location}}
#'
#' @param plot_type "interaction", "barplot" or "score"
#'
#' @param nb_parameters_per_plot number of parameter per plot to display
#'
#' @param ... further arguments passed to or from other methods
#'
#' @details
#' S3 method.
#' See example in the book: https://priviere.github.io/PPBstats_book/family-2.html#model-1
#'
#' @return
#' A list with ggplot object depending on plot_type.
#' For each plot_type, it is a list of three elements being lists with as many elements as environment.
#' For each element of the list, there are as many graph as needed with \code{nb_parameters_per_plot} parameters per graph.
#' \itemize{
#' \item barplot :
#'  \itemize{
#'   \item data_mean_comparisons : only environments where all MCMC converge are represented.
#'   Letters are displayed on each bar. Parameters that do not share the same letters are different regarding type I error (alpha) and alpha correction.
#'   The error I (alpha) and the alpha correction are displayed in the title.
#'   alpha = Imp means that no differences were possible to find.
#'
#'   \item data_env_with_no_controls : only environments where there were no controls are represented.
#'   \item data_env_whose_param_did_not_converge : only environments where MCMC did not converge are represented.
#'   }
#'
#'  \item interaction :
#'  \itemize{
#'   \item data_mean_comparisons : only environments where all MCMC converge are represented.
#'   The error I (alpha) and the alpha correction are displayed in the x.axis under the form "alpha | alpha correction".
#'   alpha = Imp means that no differences were possible to find.
#'   \item data_env_with_no_controls : only environments where there were no controls are represented.
#'   \item data_env_whose_param_did_not_converge : only environments where MCMC did not converge are represented.
#'   }
#'
#'  \item score : The score is set according to which group the entry was allocated.
#'  An high score means that the entry was in a group with an high mean.
#'  A low score means that the entry was in a group with an low mean.
#'  In the legend, the score goes from 1 (first group) to the number of groups of significativity.
#'  The error I (alpha) and the alpha correction are displayed in the x.axis under the form "alpha | alpha correction".
#'  alpha = Imp means that no differences were possible to find.
#'  }
#'
#' @author Pierre Riviere
#'
#' @seealso \code{\link{mean_comparisons.check_model_bh_intra_location}}
#'
#' @export
#'
#' @import dplyr
#' @import plyr
#' @import ggplot2
#' @importFrom methods is
#'
plot.mean_comparisons_model_bh_intra_location <- function(
  x,
  plot_type = c("interaction", "barplot", "score"),
  nb_parameters_per_plot = 8, ...
  ){

  # 1. Error message ----------
  plot_type <- match.arg(plot_type, several.ok = FALSE)

  # 2. get data ----------
  all_data = list(
    "data_mean_comparisons" = x$data_mean_comparisons,
    "data_env_with_no_controls" = x$data_env_with_no_controls,
    "data_env_whose_param_did_not_converge" = x$data_env_whose_param_did_not_converge
  )

  # 3. get ggplot ----------

  # 3.1. function used in the code ----------

  get.loc.year = function(data, nb_parameters_per_plot){
    s = median_year = NULL # to avoid no visible binding for global variable

    if( length(data) > 0 ) {
      dtmp = data.frame()
      for(i in 1:length(data)){ dtmp = rbind.data.frame(dtmp, data[[i]]$mean.comparisons) }
      data = dtmp

      d_loc = splitter_d(data, .(location))

      d_loc_b = lapply(d_loc, function(x){

        data_year = max(as.numeric(as.character(x$year)))
        # Arrange from the more recent year and by number of pop present in a year
        t = table(x$entry, x$year)
        t = as.data.frame.matrix(t)
        t$s = apply(t, 1, sum)
        t$median_year = unlist(lapply(rownames(t),function(y){
          ifelse(nrow(x[x$year %in% data_year & x$entry %in% y,])>0,x[x$year %in% data_year & x$entry %in% y,"median"],NA)
        }))

        vec = NULL
        for(i in (ncol(t)-2):1){
          tmp = t[which(t[,i] > 0),]
          tmp$rn = rownames(tmp)
          tmp = arrange(tmp, -s)
          vec = rbind(vec, tmp[,c("rn","median_year")])
        }
        vec = dplyr::arrange(vec,-median_year)
        vec = vec[!duplicated(vec$rn),"rn"]
        ee = c(1:length(vec)); names(ee) = vec
        x$split = ee[x$entry]
        x = dplyr::arrange(x, split)

        seq_nb_para = unique(c(seq(1, max(x$split), nb_parameters_per_plot), max(x$split)*2))
        for(i in 1:(length(seq_nb_para) - 1) ) { x$split[seq_nb_para[i] <= x$split & x$split < seq_nb_para[i+1]] = i }
        x_split = splitter_d(x, .(split))
        return(x_split)
      } )
    } else { d_loc_b = NULL }

    return(d_loc_b)
  }



  # 3.2. run function for barplot ----------

  if( plot_type == "barplot") {
    if(round(nb_parameters_per_plot/2) != nb_parameters_per_plot/2){nb_parameters_per_plot = nb_parameters_per_plot-1}

    fun_barplot = function(data, nb_parameters_per_plot){
      parameter = group = parameter = NULL # to avoid no visible binding for global variable

      d_env_b = lapply(data, function(x){
          x = x$mean.comparisons
          x = dplyr::arrange(x, median)
          x$max = max(x$median, na.rm = TRUE)
          x$split = add_split_col(x, nb_parameters_per_plot)
          x_split = splitter_d(x, .(split))
          return(x_split)
        } )
      
      OUT = lapply(d_env_b, function(x){
          out = lapply(x, function(dx){
            p = ggplot(dx, aes(x = reorder(parameter, median), y = median)) + geom_bar(stat = "identity")

            if(attributes(data)$PPBstats.object == "data_mean_comparisons") {
              # Add letters of significant groups
              p = p + geom_text(data = dx, aes(x = reorder(parameter, median), y = median/2, label = groups), angle = 90, color = "white")
              p = p + ggtitle(paste(dx[1, "environment"], "\n alpha = ", dx[1, "alpha"], "; alpha correction :", dx[1, "alpha.correction"])) + ylab("")
            }

            if(attributes(data)$PPBstats.object == "data_env_with_no_controls" |
               attributes(data)$PPBstats.object == "data_env_whose_param_did_not_converge") {
              p = p + ggtitle(dx[1, "environment"]) + ylab("")
            }

            p = p + xlab("") + theme(axis.text.x = element_text(angle = 90)) + ylim(0, dx[1,"max"])
            return(p)
          })
          return(out)
        })
      names(OUT) = names(d_env_b)
      OUT=lapply(OUT,function(x){if(class(x) == "list"){if(length(x) ==0){x=NULL}else{return(x)}}else{return(x)}})
      return(OUT)
      }

    out = lapply(all_data, fun_barplot, nb_parameters_per_plot)
    names(out) = names(all_data)

  }

  # 3.3. run function for score ----------

  if(plot_type == "score") {

    d_loc_out = get.loc.year(all_data$data_mean_comparisons, nb_parameters_per_plot)

    if( !is.null(d_loc_out) ) {

      out = lapply(d_loc_out, function(x){

        # assign a number according to the group
        get_score = function(env){
          vec_letters = sort(
            unique(
              unlist(
                sapply(as.character(env[,"groups"]),
                       function(x){unlist(strsplit(x, ""))})
              )
            )
          )

          SCORE = c(1:length(vec_letters))
          names(SCORE) = vec_letters

          GP = as.character(env[,"groups"])
          score = NULL
          for(gp in GP){
            score = c(score, mean(SCORE[unlist(strsplit(gp, ""))], na.rm = TRUE))
          }

          return(score)
        }


        t = NULL
        for(i in 1:length(x)) { t = c(t, x[[i]]$year) }
        all_year = unique(t)

        all_x = x[[1]]
        if( length(x) > 1 ){
          for(i in 2:length(x)) { all_x = rbind.data.frame(all_x, x[[i]]) }
        }
        all_score = unique(sort(get_score(all_x)))

        lapply(x, function(env, all_year, all_score){
          median_text = group = NULL # to avoid no visible binding for global variable

          env = dplyr::arrange(env, -median)

          # add missing year in order to have the same x axis in all plots
          t = table(env$entry, env$year)
          entry = rownames(t)
          year = colnames(t)
          year_to_add = all_year[!is.element(all_year, year)]

          if( length(year_to_add) > 0 ){
            env = rbind.data.frame(
              env,
              data.frame(
                parameter = as.factor(NA),
                median = as.numeric(NA),
                groups = as.factor(NA),
                nb_group = as.numeric(NA),
                alpha = as.numeric(rep(env[1, "alpha"], times = length(year_to_add))),
                alpha.correction = as.factor(as.character(rep(env[1, "alpha.correction"], times = length(year_to_add)))),
                entry = as.character(rep(entry, times = length(year_to_add))),
                environment = as.character(NA),
                location = as.character(NA),
                year = as.character(rep(year_to_add, each = length(entry))),
                split = as.numeric(NA)
              )
            )
          }


          env$group = get_score(env) # group instead of score for the legend
          env$median_text = as.character(round(env$median, 1))
          env = dplyr::arrange(env,-median)
          env = dplyr::arrange(env,-as.numeric(year))
          env$entry = factor(env$entry,levels= env$entry[!duplicated(env$entry)][length(env$entry[!duplicated(env$entry)]):1])
          p = ggplot(env, aes(y = entry, x = year, label = median_text, fill = group))
          p = p +  geom_tile() + geom_text()
          p = p + scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = mean(all_score), na.value = "transparent", limits = range(all_score) )
          p = p + xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
          p = p + ggtitle(paste(env[1, "location"], "; alpha = ", env[1, "alpha"], "; correction : ", env[1, "alpha.correction"], sep = ""))
          return(p)
        }, all_year, all_score)
      })
      names(out) = names(d_loc_out)
    } else { out = NULL }

  }


  # 3.4. run function for interaction ----------
  if(plot_type == "interaction") {

    fun_interaction = function(data, nb_parameters_per_plot){
      alpha.info_year = entry = year = NULL # to avoid no visible binding for global variable

      d_loc_out = get.loc.year(data, nb_parameters_per_plot)

      if( !is.null(d_loc_out) ) {
        out = lapply(d_loc_out, function(x){
          lapply(x, function(x_loc) {
            # Same scale for all the plots
            ymin = min(x_loc$median, na.rm = TRUE)
            ymax = max(x_loc$median, na.rm = TRUE)

            if( attributes(data)$PPBstats.object == "data_mean_comparisons" ){
              alpha.info = paste(x_loc$alpha, "|", x_loc$alpha.correction)
              x_loc$alpha.info_year = as.factor(paste(x_loc$year, alpha.info, sep = " - "))
            }

            if( attributes(data)$PPBstats.object == "data_env_with_no_controls" |
                attributes(data)$PPBstats.object == "data_env_whose_param_did_not_converge"
            ) {
              x_loc$alpha.info_year = x_loc$year
            }

            p = ggplot(x_loc, aes(y = median, x = alpha.info_year, colour = entry, group = entry))
            p = p + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line") + ggtitle(x_loc[1, "location"])
            p = p + xlab("") + ylab("") + theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.title = element_blank())

            x_loc_year = splitter_d(x_loc, .(year))

            # Put lines for significant groups
            if( attributes(data)$PPBstats.object == "data_mean_comparisons" ){
              SEG = NULL

              vec_letters = c(letters, paste(rep(LETTERS, times = 15), rep(c(1:5), each = 15), sep = ""))
              gp_letters = vec_letters[1:max(x_loc$nb_group)]
              xadjust = seq(0.05, 0.9, length = 25) # it is assumed max 25 letters (i.e. groups)
              xadjust = xadjust[c(1:length(gp_letters))]
              names(xadjust) = gp_letters

              for(i in 1:length(x_loc_year)) {
                subx = droplevels(x_loc_year[[i]])

                # get rid of non sens information
                # This is useful if a germplasm is in group 'a' and 'b' alone.
                # This is possible because other germplasm in groups 'a' and 'b' are in another year
                subx = arrange(subx, median) # To get the letter in the right order
                groups = as.character(subx$groups)

                a = lapply(groups, function(x) { unlist(strsplit(x, "")) } )
                row = unique(unlist(a))
                m = matrix(0, ncol = length(groups), nrow = length(row))
                rownames(m) = row
                for(jj in 1:length(a)) { m[a[[jj]], jj] = 1 }
                m = unique(m)
                if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }

                if( nrow(m) > 1) {
                  todelete = NULL
                  for(jj in 1:ncol(m)) {
                    toget = which(m[,jj] == 1)
                    if( length(toget) >  1 ) {
                      for(ii in toget) {
                        if( sum(m[ii,]) == 1 ) { todelete = c(todelete, ii) }
                      }
                    }
                  }

                  if (!is.null(todelete)) { m = m[-todelete,] }
                  if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }
                }


                # Following code to discard redondant informations
                # For example
                # 1 1 0 0
                # 0 1 1 1
                # 0 1 1 0
                # will give
                # 1 1 0 0
                # 0 1 1 1
                # indeed, the last row brings no informations

                if( nrow(m) > 1) {
                  todelete = NULL
                  for(ii in 1:(nrow(m)-1) ) {
                    w1 = which( m[ii,] == 1 )
                    w2 = which( m[ii+1,] == 1 )
                    t = which(is.element(w1, w1[is.element(w1, w2)]))
                    test = length(w2) == length(t)

                    if( test ) { todelete = c(todelete, ii+1)}
                  }
                  if( !is.null(todelete) ) { m = m[-todelete,] }
                  if( is.vector(m) ) { m = as.data.frame(matrix(m, nrow = 1)) }
                }


                # Initialize the letters
                if( nrow(m) > 1) {
                  for(ii in 1:nrow(m)) {
                    m[ii, which(m[ii,] == 1)] = vec_letters[ii]
                    m[ii, which(m[ii,] == 0)] = ""
                    }
                } else {
                  m[1, which(m[1,] == 1)] = vec_letters[1]
                  }

                groups = apply(m, 2, function(x){paste(x, collapse="")})

                subx$groups = factor(groups)

                for(l in gp_letters) {
                  togrep = grep(l, subx[,"groups"])

                  if(length(togrep) > 0) {
                    # check if it is continuous
                    vec.togrep = NULL

                    a = togrep
                    if( length(a) > 1 ){
                      test = a[1:(length(a)-1)] + 1 == a[2:length(a)]
                      t = c(FALSE, test)
                      t[which(t)] = 1; t[which(!t)] = 0
                      b = paste("0",unlist(strsplit(paste(as.character(t[2:length(t)]),collapse=""),"0")),sep="")
                      if(t[length(t)] == 0) { b = c(b, 0) }
                      c = c(0, cumsum(sapply(b, function(x){nchar(x)})))
                      for(k in 1:(length(c)-1)) { vec.togrep = c(vec.togrep, (a[(c[k]+1):c[k+1]])) }
                    } else { vec.togrep = c(vec.togrep, togrep) }

                    toto = subx[vec.togrep, "median"]

                    y.seg = range(toto)
                    x.seg = i + xadjust[l]
                    alpha.info = paste(as.character(subx[1, "alpha"]),"|", as.character(subx[1, "alpha.correction"]))
                    seg = cbind.data.frame(ymin = y.seg[1], ymax = y.seg[2], x = x.seg, groups = paste("group", l), alpha.info, year = names(x_loc_year)[i])
                    SEG = rbind.data.frame(SEG, seg)
                  }
                }
              }

              # For germplasm alone in one group, change ymax in order to see it
              for(i in 1:nrow(SEG)){
                if( SEG$ymin[i] == SEG$ymax[i] ) { SEG$ymax[i] = SEG$ymax[i] + min(SEG$ymax)/30 }
              }

              if(!is.null(SEG)) {
                p = p + geom_segment(aes(x = x, y = ymin, xend = x, yend = ymax, group = NULL), colour =  as.numeric(as.factor(SEG$groups)), data = SEG)
                p = p + ylim(ymin - ymin/10, ymax + ymax/10) # To be sure to see the line of the groups
              }
            }

            return(p)
          })
        } )
        names(out) = names(d_loc_out)
      } else { out = NULL }

      return(out)
    }

    out = lapply(all_data, fun_interaction, nb_parameters_per_plot)

  }

  # return results
  return(out)
}
