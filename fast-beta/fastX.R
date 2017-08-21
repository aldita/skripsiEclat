################################################################
# fungsi yang digunakan bersama dalam aplikasi
################################################################
# mengganti data
changedata <- function(addCol, addColName = "") {
  if(nrow(getdata()) == nrow(addCol) && addColName[1] != "") {
    return(values[[input$datasets]][,addColName] <- addCol)
  }
}

changedata_names <- function(oldnames, newnames) {
  upnames <- colnames(values[[input$datasets]])
  upnames[which(upnames %in% oldnames)] <- newnames
  return(colnames(values[[input$datasets]]) <- upnames)
}

inChecker <- function(tocheck) {
  ifelse(sum(tocheck %in% varnames()) < length(tocheck), return(NULL), return('OK'))
}

# fungsi untuk mengambil data dari varaibel global berdasarkan pilihan user
getdata <- reactive({
  values[[input$datasets]]
})

# mendapatkan kelas dari data
getdata_class <- reactive({
  # don't use isolate here or values won't change when the dataset is changed
  cls <- sapply(getdata(), function(x) class(x)[1])
  gsub("ordered","factor", cls)
})

getdata_class_ts <- reactive({
  # don't use isolate here or values won't change when the dataset is changed
  cls <- sapply(ts(getdata()), function(x) class(x)[1])
  gsub("ordered","factor", cls)
})

# mengambil nama variabel dari data
varnames <- reactive({
  dat <- getdata_class()
  vars <- names(dat)
  names(vars) <- paste(vars, " {", dat, "}", sep = "")
  vars
})

varnames_ts <- reactive({
  dat <- getdata_class_ts()
  vars <- names(dat)
  names(vars) <- paste(vars, " {", dat, "}", sep = "")
  vars
})
# untuk mengubah tanggal menjadi karakter
date2character <- reactive({
  date2character_dat(getdata())
})

# proses mengubah tanggal menjadi karakter
date2character_dat <- function(dat) {
  # xtable tidak bisa memakai tipe dates
  isDate <- c(sapply(dat, is.Date))
  dat[,isDate] <- sapply(dat[,isDate], as.character)
  dat
}

################################################################
# fungsi untuk memudahkah membuat input dan output
################################################################
# mengatur lebar plot sesuai dengan input$viz_plot_width di visualize.R, defaultnya 650px
plotWidth <- function() {
  ifelse(is.null(input$viz_plot_width), return(values$plotWidth), return(input$viz_plot_width))
}

# mengatur tinggi plot sesuai dengan input$viz_plot_height di visualize.R, defaultnya 650px
plotHeight <- function() {
  ifelse(is.null(input$viz_plot_width), return(values$plotHeight), return(input$viz_plot_height))
}

statPanel <- function(fun_name, rfun_label, fun_label, widthFun, heightFun) {
  
  if(isolate(input$nav_fast) != fun_name) return()
  
  sum_name <- paste0("summary_", fun_label)
  plot_name <- paste0("plots_", fun_label)
  
  # Generate output for the summary tab
  output[[sum_name]] <- renderPrint({
    
    result <- get(rfun_label)()
    # when no analysis was conducted (e.g., no variables selected)
    if(is.character(result)) return(cat(result,"\n"))
    
    get(sum_name)()
  })
  
  # Generate output for the plots tab
  output[[plot_name]] <- renderPlot({
    
    result <- get(rfun_label)()
    # when no analysis was conducted (e.g., no variables selected)
    if(is.character(result)) return(plot(x = 1, type = 'n', main=result, axes = FALSE, xlab = "", ylab = ""))
    
    get(plot_name)()
  }, width=get(widthFun), height=get(heightFun))
  
  return(tabsetPanel(
    id = paste0("tabs_",fun_label),
    tabPanel("Summary", verbatimTextOutput(sum_name)),
    tabPanel("Plots", conditionalPanel(condition="!$('html').hasClass('shiny-busy')",
                                       plotOutput(plot_name, height = "100%")))
  ))
}

statTabPanel <- function(menu_name, fun_name, rfun_label, fun_label, widthFun = "plotWidth", heightFun = "plotHeight") {
  isolate({ 
    sidebarLayout(
      sidebarPanel(
        # based on https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/PzlSAmAxxwo
        div(class = "busy",
            p("Calculation in progress ..."),
            img(src="ajaxloaderq.gif")
        ),
        wellPanel(
          HTML(paste("<label><strong>Menu:",menu_name,"</strong></label>")),
          HTML(paste("<label><strong>Tool:",isolate(input$nav_fast),"</strong></label>")),
          HTML(paste("<label><strong>Data:",input$datasets,"</strong></label>"))
        ),
        uiOutput(paste0("ui_",fun_label))
      ),
      mainPanel(
        statPanel(fun_name, rfun_label, fun_label, widthFun, heightFun)
      )
    )
  })	
}

#List direktori
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}


################################################################
# fungsi untuk membuat popup
################################################################
bsModal1 <- function(id, title, trigger, ..., href) {
  
  mo <- tags$div(class = "modal sbs-modal hide fade", id = id,
                 "data-trigger" = trigger,
                 tags$div(class = "modal-header",
                          tags$button(Type = "button", class = "close",
                                      "data-dismiss" = "modal", HTML("&times;")),
                          tags$h3(title)),
                 body <- tags$div(class = "modal-body")
  )
  
  if(!missing(href)) {
    mo <- addAttribs(mo, "data-remote" = href)
  } else {
    mo$children[[2]] <- tagAppendChildren(mo$children[[2]], list = list(...))
  }
  
  return(mo)
  
}

toggleModal1 <- function(session, modalId) {
  
  session$sendInputMessage(modalId, list(toggle = TRUE))
  
}

#####FUNGSI POP UP BARU UNTUK SHINY 0.12
#Edit dari shinyBS 0.61 (twitter bootstrap 3)
bsModal2 <- function (id, title, trigger, ..., size, footer=list()){
  if (!missing(size)) {
    if (size == "large") {
      size = "modal-lg"
    }
    else if (size == "small") {
      size = "modal-sm"
    }
    size <- paste("modal-dialog", size)
  }
  else {
    size <- "modal-dialog"
  }
  bsTag <- tags$div(class = "modal sbs-modal fade", 
                    id = id, tabindex = "-1", 'data-sbs-trigger' = trigger, 
                    tags$div(class = size, tags$div(class = "modal-content", 
                                                    tags$div(class = "modal-header", tags$button(type = "button", 
                                                                                                 class = "close", 'data-dismiss' = "modal", tags$span(HTML("&times;"))),
                                                             tags$h4(class = "modal-title", title)), 
                                                    tags$div(class = "modal-body", list(...)), 
                                                    tags$div(class = "modal-footer", footer))))}

toggleModal2 <- function (session, modalId, toggle = "toggle"){
  session$sendInputMessage(modalId, list(toggle = toggle))
}

########FUNGSI SHARE KE FORUM
SHAREfile_names<-function(n=1, length=12) {
  randomString<-c(1:n) 
  for (i in 1:n) {
    randomString<- paste(sample(c(0:9, letters,LETTERS), length, replace=TRUE), collapse="")
  }
  return(randomString)
}

#menangkap elemen yang ingin dishare
getFromCapture <- function(tabel){
  stre <- ""
  hasilCapture <- capture.output(tabel)
  for(i in 1:length(hasilCapture)){
    stre <- paste(stre, hasilCapture[i], "<br/>")
  }
  return(stre)
}

#kondisi salah
idTidakDitemukan <- function(){
  createAlert(session, "modalAlert", alertId = "gagalAlert", title = "Incorrect Username/Password", content = NULL, style = "danger")
  updateTextInput(session, "usernameFCMForum", value="")
  updateTextInput(session, "passwordFCMForum", value="")
}

#query query pendukung
#get user id
getUserIDFromDB <- function(usrnm, psswd){
  queryID <- paste0("SELECT id FROM tbl_users WHERE username = '",usrnm,"' AND password = MD5('",psswd,"')")
  theUID <- dbGetQuery(mydb, queryID)
  return(theUID)
}

#get id dari last post
getLastPostID <- function(){
  queryPostID <- paste0("SELECT MAX(last_post_id) FROM bbii_topic")
  thepostID <- suppressWarnings(dbGetQuery(mydb, queryPostID))
  thepostID <- thepostID+1
  return(thepostID)
}

#get topic id
getTopicID <- function(){
  queryTopicID <- paste0("SELECT MAX(id) FROM bbii_topic")
  theTopicID <- suppressWarnings(dbGetQuery(mydb, queryTopicID))
  theTopicID <- theTopicID+1
  return(theTopicID)
}

#get num of post di forum
getNumPosts <- function(idForum){
  queryNumPosts <- paste0("SELECT num_posts FROM bbii_forum WHERE id = '",idForum,"'")
  theNumPosts <- suppressWarnings(dbGetQuery(mydb, queryNumPosts))
  theNumPosts <- theNumPosts+1
  return(theNumPosts)
}

#get num of topics di forum
getNumTopics <- function(idForum){
  queryNumTopics <- paste0("SELECT num_posts FROM bbii_forum WHERE id = '",idForum,"'")
  theNumTopics <- suppressWarnings(dbGetQuery(mydb, queryNumTopics))
  theNumTopics <- theNumTopics+1
  return(theNumTopics)
}

#get individual post count
getPostCount <- function(id){
  queryPostCount <- paste0("SELECT posts FROM bbii_member WHERE id='",id,"'")
  postCount <- suppressWarnings(dbGetQuery(mydb, queryPostCount))
  postCount <- postCount+1
  return(postCount)
}

#query-query SHARE
createTopic <- function(theTopicID, idForum, theUID, subject, bannerPath, thepostID){
  query <- paste0("INSERT INTO fast_backup.bbii_topic (id, forum_id, user_id, title, image, first_post_id, last_post_id, num_replies, num_views, approved, locked, sticky, global, moved, upvoted) VALUES ('",theTopicID,"', '",idForum,"', '",theUID,"', '",subject,"','",bannerPath,"' ,'",thepostID,"', '",thepostID,"', '0', '0', '1', '0', '0', '0', '0', '0')")
  return(query)
}

postContent <- function(thepostID, subject, content, summaryForum, theUID, theTopicID, idForum){
  query <- paste0("INSERT INTO fast_backup.bbii_post (id, subject, content, resumes, user_id, topic_id, forum_id, ip, create_time, approved, change_id, change_time, change_reason, upvoted) VALUES ('",thepostID,"', '",subject,"', '",content,"', '<p>",summaryForum,"</p>', '",theUID,"', '",theTopicID,"', '",idForum,"', NULL, NULL, '1', NULL, NULL, NULL, '0')")
  return(query)
}

updateForum <- function(theNumPosts, theNumTopics, thepostID, idForum){
  query <- paste0("UPDATE fast_backup.bbii_forum SET num_posts = '",theNumPosts,"', num_topics = '",theNumTopics,"', last_post_id = '",thepostID,"' WHERE bbii_forum.id = '",idForum,"'")
  return(query)
}

logTopic <- function(theUID, theTopicID, idForum, thepostID){
  query <- paste0("INSERT INTO fast_backup.bbii_log_topic (member_id, topic_id, forum_id, last_post_id) VALUES ('",theUID,"','",theTopicID,"','",idForum,"','",thepostID,"')")
}

updatePostCount <- function(thePostCount, theUID){
  query <- paste0("UPDATE fast_backup.bbii_member SET posts = '", thePostCount, "' WHERE id='",theUID,"'")
  return(query)
}