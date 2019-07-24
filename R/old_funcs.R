
apl <- function ( n = 0, d=NA, h=NULL, dup=FALSE ) {
  return(list('count'=n, 'default'=d, help=h, duplicate_ok=dup));
}

parse.AFNI.args <- function ( args, params = NULL,
                              other_ok=TRUE,
                              verb = 0) {
  #for (i in 1:length(args)) {
  #   cat (i, args[[i]],'\n');
  #}
  if (is.null(args)) return(NULL)

  if (!is.null(params)) {
    allowed_options <- sort(names(params));
    duplicate_okvec <- vector('character');
    for (i in 1:1:length(params)) {
      pl <- params[i][[1]];
      if (pl['duplicate_ok'][[1]]) {
        duplicate_okvec <- c(duplicate_okvec, names(params)[i])
      }
    }
  } else {
    allowed_options <- vector('character');
  }

  #Add global args like -overwrite
  allowed_options <- c(allowed_options, "-overwrite")

  #find locations of -*
  ii <- grep ('^-.*', args);
  iflg <- vector('numeric')
  if (length(ii) > 0) {
    for (i in 1:length(ii)) {
      if (!is.num.string(args[[ii[i]]])) {
        if (!length(allowed_options)) {
          iflg <- append(iflg, ii[i]);
        } else { #Make sure it is an acceptable name
          if (length(which(args[[ii[i]]] == allowed_options))) {
            iflg <- append(iflg, ii[i]);
          }
        }
      }
    }
  }

  if (verb) note.AFNI(paste(args[iflg]))

  ops = list()
  used <- vector('logical', length(args));
  if (length(iflg)) {
    iflg <- append(iflg,length(args)+1)
    #store results
    nm <- vector('character');
    for (i in 1:(length(iflg)-1)) {
      if (0) { # Why remove the -?, makes things inconsistent elsewhere
        #newnm <- strsplit(args[[iflg[i]]],'-')[[1]][2]
      } else newnm <- args[[iflg[i]]]

      if (length(nm) && length(which(newnm == nm)) &&
          (newnm != '-gltLabel') && (newnm != '-gltCode') &&  # 10/18/2012 GC: added this line for 3dMVM
          (newnm != '-glfLabel') && (newnm != '-glfCode') &&  # 12/22/2014 GC: added this line for 3dMVM
          (!length(duplicate_okvec) ||
           length(which(iflg[i] == duplicate_okvec))) ){
        warning(paste('option ', newnm, 'already specified.\n'),
                immediate. = TRUE);
        show.AFNI.args(ops)
        return(NULL);
      }
      #nm <- append(nm, newnm)

      used[iflg[i]] <- TRUE;
      istrt = iflg[i]+1;
      pp <- vector('character');
      if (istrt <= length(args) && istrt != iflg[i+1]) {
        iend <- max(c(iflg[i+1]-1, istrt))
        for (ii in istrt:iend) {
          pp <- append(pp, args[[ii]]);
          used[ii] <- TRUE;
        }
      }
      #create a cleaned up string
      pp <- paste(pp, collapse = ' ')
      if (  length(grep('^".*"$',pp)) || #Quoted string, do not split
            length(grep("^'.*'$",pp)) || #Quoted string, do not split
            length(grep("^1D:.*$",pp)) || #1D: string, do not split
            length(grep("^R:.*$",pp)) ) {
      } else {
        if (verb) {
          note.AFNI(sprintf("Splitting >>>%s<<<", pp))
        }
        pp <- strsplit(clean.args.string(pp), ' ')
      }
      if((newnm != '-gltLabel') && (newnm != '-glfLabel') && (newnm != '-gltCode') && (newnm != '-glfCode')) { # 12/22/2014 GC: added this line for 3dMVM
        ops <- c(ops, (pp))
        names(ops)[length(ops)] <- newnm
      } else if(length(which(newnm == nm)))
        ops[[newnm]] <- c(ops[[newnm]], pp) else {
          ops <- c(ops, list(pp))
          names(ops)[length(ops)] <- newnm
        }
      nm <- append(nm, newnm)
    }
  }
  #cleanup
  if (length(ops)) {
    for (i in 1:length(ops)) {
      #ops[[i]] <- clean.args.string(ops[[i]])
    }
  }

  #numeric changes
  if (length(ops))
    for (i in 1:length(ops))
      if(!is.list(ops[[i]])) if (is.num.string(ops[[i]]))
        ops[[i]] <- as.numeric(ops[[i]])

  #defaults
  pp <- c(args[used == FALSE])
  ops <- c (ops, list("other"=pp));

  #add allowed options
  ops <- c (ops, list("allowed_options"=allowed_options));

  if (!other_ok) {
    if (length(ops[['other']])) {
      err.AFNI(paste('Illegal parameters on command line:\n',
                     '      ', ops['other'],
                     '\nTry -allowed_options, or -help for details\n',
                     '\n'));
      exit.AFNI(1);
    }
  }

  #check
  if (!check.AFNI.args(ops, params)) {
    return(NULL);
  } else {
    return(ops);
  }
}


is.num.string <- function(ss) {
  if (is.null(ss) || !length(ss) || ss == '' ||
      is.null(tryCatch(as.numeric(ss),
                       warning=function(ex) {}))) {
    return(0);
  } else {
    return(1);
  }
}

#------------------------------------------------------------------
# Global Variables
#------------------------------------------------------------------
BATCH_MODE <<- 0  #Initialize batch mode flag to 0
R_io <<- -1
SHOW_TRC <<- FALSE

#------------------------------------------------------------------
# Functions for library loading
#------------------------------------------------------------------
find.in.path <- function(file) { #Pretty much same as first.in.path
  ff <- paste(strsplit(Sys.getenv('PATH'),':')[[1]],'/', file, sep='')
  ff <- ff[lapply(ff,file.exists)==TRUE];
  aa <- gsub('//','/',ff[1], fixed=TRUE)
  if (is.na(aa)) aa <- NULL
  return(aa)
}

#Return cat's file option to format the help output
#for TXT or sphinx purposes
help.cat.file.AFNI <- function (pname=NULL, targ='TXT') {
  if (is.null(pname)) {
    err.AFNI("NULL name for help function");
    return("");
  }
  if (targ == 'TXT') {
    dopt = '-hdoc_2_txt';
  } else if (targ == 'SPX') {
    dopt = '-hdoc_2_spx';
  } else if (targ == 'ASPX') {
    dopt = '-hdoc_2_aspx';
  } else if (targ == 'RAW') {
    return("");
  } else {
    warn.AFNI(paste("targ", targ,"unknown. Assuming 'TXT'"));
    dopt = '-hdoc_2_txt';
  }
  return(paste("|apsearch ", dopt ,pname,"-"));
}

#print warnings a la AFNI
prompt.AFNI <- function (str='I await', choices=c('y','n'), vals=NULL) {
  if (!is.null(vals) && length(vals) != length(choices)) {
    err.AFNI(paste("Have ", length(choices), "options, but",
                   length(vals), "values to return"));
    return(0)
  }
  choices[1]<-toupper(choices[1])
  kk<-vector(length=0);
  spr <- paste(str," [",paste(choices, collapse='|'),"]:", sep='')
  if (BATCH_MODE) { #Only two choices available for this one!
    if (length(choices)!=2) {
      err.AFNI("This one can't run in batch mode for more than 2 choices")
      return(0)
    }
    ss <- sys.AFNI(paste(
      "prompt_user -pause '", spr,"'", sep='' ))
    if (ss$out == "0") return(2) #The not default
    else return(1) #The default
  } else {
    while (length(kk) == 0) {
      cat(spr)
      bb <- readLines(n=1)
      if (bb == '') {
        kk <- 1;
      } else {
        kk <-which(tolower(choices) == tolower(bb))
      }
    }
    if (!is.null(vals)) {
      return(vals[kk])
    } else {
      return(kk)
    }
  }
  return(0)
}

set.AFNI.msg.trace <- function (vv=FALSE) { SHOW_TRC <<- vv }

warn.AFNI <- function (str='Consider yourself warned',
                       callstr=NULL,
                       newline=TRUE) {
  if (is.null(callstr)) {
    if (SHOW_TRC) callstr <- who.called.me(TRUE)
    else callstr <- ''
  }
  nnn<-''
  if (newline) nnn <- '\n'
  if (BATCH_MODE) ff <- stderr()
  else ff <- ''
  cat(  '\n', 'oo Warning: ',  callstr,'\n   ',
        paste(str, collapse=''), nnn,
        sep='', file = ff);
}

err.AFNI <- function (str='Danger Danger Will Robinson',
                      callstr=NULL,
                      newline=TRUE) {
  if (is.null(callstr)) {
    if (SHOW_TRC) callstr <- who.called.me(TRUE)
    else callstr <- ''
  }
  nnn<-''
  if (newline) nnn <- '\n'
  if (BATCH_MODE) ff <- stderr()
  else ff <- ''
  cat(  '\n', '** Error: ',  callstr,'\n   ',
        paste(str, collapse=''), nnn,
        sep='', file = ff);
}

errex.AFNI <- function (str='Alas this must end',
                        callstr=NULL, newline=TRUE) {
  if (is.null(callstr)) {
    if (SHOW_TRC) callstr <- who.called.me(TRUE)
    else callstr <- ''
  }
  err.AFNI(str,callstr, newline)
  exit.AFNI(str='\n   Execution halted',stat=1)
}

note.AFNI <- function (str='May I speak frankly?',
                       callstr=NULL, newline=TRUE, tic=1,
                       trimtrace=30) {
  if (is.null(callstr)) {
    if (SHOW_TRC)  callstr <- who.called.me(TRUE, trim=trimtrace)
    else callstr <- ''
  }
  nnn<-''
  if (newline) nnn <- '\n'
  if (BATCH_MODE) ff <- stderr()
  else ff <- ''
  if (tic == 1) {
    tm <- format(Sys.time(), " @ %H:%M:%S")
  } else if (tic==2) {
    tm <- format(Sys.time(), " @ %a %b %d %H:%M:%S %Y")
  } else tm <- ''

  cat(  '\n', '++ Note: ',  callstr,
        sprintf('%s\n   ', tm),
        paste(str, collapse=''),nnn,
        sep='', file = ff);
}

exit.AFNI <- function(str='The piano has been drinking.', stat=0) {
  if (BATCH_MODE) {
    quit(save='no', status = stat);
  } else {
    #note.AFNI(str)
    stop(str)
  }
}


clean.args.string <- function(ss) {
  if (is.list(ss) || length(ss) > 1) {
    warning(paste('Function only works on single strings',
                  str(ss),'\n', sep=''),
            immediate.=TRUE);
    return(NULL);
  }
  #remove trailing whites
  ss <- sub('^[[:space:]]*','',ss);
  ss <- sub('[[:space:]]*$','',ss);
  #remove multiple whites
  ss <- gsub('[[:space:]]+',' ',ss);
  #treat = nicely
  ss <- gsub('[[:space:]]*=[[:space:]]*','=',ss)
  return(ss)
}

deblank.string <- function(s, start=TRUE, end=TRUE, middle=FALSE) {
  if (end) {
    s = sub('[[:space:]]+$','',s);
  }
  if (start) {
    s = sub('^[[:space:]]+','',s);
  }
  if (middle) {
    s = gsub('[[:space:]]+',' ',s);
  }
  return(s);
}

pad.string.lines <- function(s, pre='   ', post=NULL) {
  if (!is.null(pre)) {
    s = sub('^',pre,s);
    s = gsub('\n',sprintf('\n%s',pre),s);
  }
  if (!is.null(post)) {
    s = sub('$',post,s);
    s = gsub('\n',sprintf('%s\n',post),s);
  }

  return(s);
}

trim.string <- function (s, nchar=32, left=TRUE, strim='...')
{
  ss <- strsplit(s,'')[[1]]
  if (length(ss)>nchar) {
    #try deblanking
    s <- deblank.string(s)
    ss <- strsplit(s,'')[[1]]
    nc <- length(ss)
    if (nc>nchar) {
      #browser()
      nstrim = length(strsplit(strim,'')[[1]])
      if (left) {
        ns <- nc - nchar - nstrim
        if (ns > nstrim) {
          ss <- ss[ns:nc]
          s<-paste(strim,paste(ss,collapse=''), sep='')
        }
        return(s)
      }else {
        ns <- nchar - nstrim
        ss <- ss[1:ns]
        s<-paste(paste(ss,collapse=''), strim, sep='')
      }
    } else return(s)
  } else return(s)
}

as.num.vec <- function(ss, addcount=TRUE, sepstr='.', reset=FALSE) {
  if (is.list(ss) || length(ss) > 1) {
    warning(paste('Function only works on single strings',
                  str(ss),'\n', sep=''),
            immediate.=TRUE);
    return(NULL);
  }
  ss <- clean.args.string(ss)
  dd <- strsplit(ss,' ')[[1]];
  nn <- vector('numeric');
  ww <- vector('character');
  lastname <- '.v'
  valnum <- 0
  for (ii in 1:length(dd)) {
    vv <- strsplit(dd[ii],'=')[[1]];
    if (length(vv) > 1) {
      valnum <- valnum+1
      ll <- vv[1]
      vv <- as.numeric(vv[length(vv)]);
      if (is.na(vv)) { return(NULL); }
      lastname <- ll
    } else {
      valnum <- valnum+1
      wrn <- getOption('warn'); options(warn=-1);
      vv <- as.numeric(vv[1]); options(warn=wrn);
      if (is.na(vv)) { return(NULL); }
      if (addcount) {
        sfnd <- paste(lastname, sepstr,'[[:digit:]]*$', sep='', collapse='')
        if (!reset) {
          ifnd <- grep(sfnd,ww);
        } else {
          ifnd <- grep(sfnd,ww[length(ww)]);
          if (length(ifnd)) {
            ifnd <- length(ww);
          } else {
            valnum <- 1
          }
        }
        if (length(ifnd)) {
          lastval <- strsplit(ww[ifnd[length(ifnd)]],
                              paste(lastname, sepstr,sep=''))[[1]];
          if (lastval[length(lastval)] == '') {
            valnum <- 1
          } else {
            valnum <- as.numeric(lastval[length(lastval)]) + 1
          }
        }
        ll <- paste(lastname,sepstr, as.numeric(valnum), sep='')
      } else {
        ll <- paste(lastname, sep='')
      }
    }

    nn <- c(nn,vv)
    ww <- c(ww,ll)
  }
  names(nn) <- ww
  return(nn)
}

as.char.vec <- function(ss) {
  if (is.list(ss) || length(ss) > 1) {
    warning(paste('Function only works on single strings',
                  str(ss),'\n', sep=''),
            immediate.=TRUE);
    return(NULL);
  }
  ss <- clean.args.string(ss)
  dd <- strsplit(ss,' ')[[1]];
  nn <- vector('character');
  ww <- vector('character');
  for (ii in 1:length(dd)) {
    vv <- strsplit(dd[ii],'=')[[1]];
    if (length(vv) > 1) ll <- vv[1]
    else ll <- paste('v',as.character(vv[1]), sep='')

    vv <- as.character(vv[length(vv)]);
    if (is.na(vv)) { return(NULL); }

    nn <- c(nn,vv)
    ww <- c(ww,ll)
  }
  names(nn) <- ww
  return(nn)
}

check.AFNI.args <- function ( ops, params = NULL, verb=0) {
  if (!is.null(params) && !is.null(ops)) {
    for (i in 1:length(ops)) {
      if (verb) {
        str(names(ops)[i])
        str(params[names(ops)[i]][[1]])
        cat('\nChecking on ', paste(ops[[i]],collapse=','),'\n');
      }
      ipar <- which(names(ops)[i] == names(params));
      if (length(ipar)) {
        pp <- params[ipar[1]][[1]]['count'][[1]];
        opsvec <- ops[[i]];
        if (length(pp) == 1) { #exact number
          if (length(opsvec) !=  pp) {
            #browser()
            msg <- paste( 'Expecting ',pp, ' parameters for option "',
                          names(ops)[i], '".\n   Have ',
                          length(opsvec), ' parameter(s) in string "',
                          paste(opsvec, collapse = ' '),
                          '" instead.', sep = '')
            if (length(opsvec) > pp &&
                length(grep('^-[a-z,A-Z]', opsvec[1+pp]))) {
              msg <- paste( msg, '\n   NOTE that ', opsvec[1+pp],
                            ' in bad option above is not a recognized option.',
                            collapse = '', sep = '' );
            }
            err.AFNI(msg);
            return(0);
          }
        } else if (length(pp) == 2) { #range
          if (length(opsvec) <  pp[1] || length(opsvec) >  pp[2]) {
            if (pp[2] == Inf) {
              msg <- paste( 'Expecting more than ',pp[1],
                            ' parameters for option "',
                            names(ops)[i], '".\n   Have ',
                            length(opsvec), ' parameter(s) in string "',
                            paste(opsvec, collapse = ' '),
                            '" instead.', sep = '')
              if (length(opsvec) > 0 &&
                  length(grep('^-[a-z,A-Z]', opsvec[1]))) {
                msg <- paste( msg, '\n   NOTE that ', opsvec[1],
                              ' in bad option above is not a recognized option.',
                              collapse = '', sep = '' );
              }
              err.AFNI(msg);
            } else {
              msg <- paste( 'Expecting ',pp[1], ' to ', pp[2],
                            ' parameters for option "',
                            names(ops)[i], '".\n   Have ',
                            length(opsvec), ' parameter(s) in string "',
                            paste(opsvec, collapse = ' '),
                            '" instead.', sep = '');
              if (length(opsvec) > pp[2] &&
                  length(grep('^-[a-z,A-Z]', opsvec[1+pp[2]]))) {
                msg <- paste( msg, '\n   NOTE that ', opsvec[1+pp[2]],
                              ' in bad option above is not a recognized option.',
                              collapse = '', sep = '' );
              }
              err.AFNI(msg);
            }
            return(0);
          }

        } else {
          warning(paste( 'I do not know what to do here'),
                  immediate. = TRUE);
          return(0);
        }
      } else {
        #ok
      }
    }
  }
  return(1); #all ok
}
