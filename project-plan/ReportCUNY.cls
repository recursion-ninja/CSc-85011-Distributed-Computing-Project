%% ReportCUNY.cls
%% Copyright 2022 Alex Washburn
%
% This work may be distributed and/or modified under the
% conditions of the LaTeX Project Public License, either version 1.3
% of this license or (at your option) any later version.
% The latest version of this license is in
%   http://www.latex-project.org/lppl.txt
% and version 1.3 or later is part of all distributions of LaTeX
% version 2005/12/01 or later.
%
%  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
%
%     *** *** *** ***       Report CUNY       *** *** *** ***
%
%  A LaTeX style for term papers for CUNY courses
%
%  -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

\LoadClass[%
a4paper,               % paper size
titlepage,
onecolumn,
11pt,
leqno,
]{article}%

\NeedsTeXFormat{LaTeX2e}
\ProvidesPackage{ReportCUNY}[2023/02/18 v1.2.1 A LaTeX style for reports of CUNY courses]

\RequirePackage{lmodern}

\PassOptionsToPackage{utf8}{inputenc}
\RequirePackage{inputenc}

\PassOptionsToPackage{T1}{fontenc}
\RequirePackage{fontenc}

\renewcommand*\familydefault{\sfdefault}

\PassOptionsToPackage{a4paper,left=0.75in,right=0.75in,top=0.75in,bottom=0.75in}{geometry}
\RequirePackage{geometry}
%
%----------------------------- Report CUNY variables -------------------------%
\global\let\VarAssetPath\empty%
\global\let\VarCourseName\empty%
\global\let\VarCourseNumber\empty%
\global\let\VarCitationFile\empty%
\global\let\VarDateD\empty%
\global\let\VarDateM\empty%
\global\let\VarDateY\empty%
\global\let\VarDepartmentAbbrev\empty%
\global\let\VarDepartmentName\empty%
\global\let\VarInstitution\empty%
\global\let\VarInstructor\empty%
\global\let\VarStudent\empty%
\global\let\VarSubtitle\empty%
\global\let\VarTitle\empty%

\global\let\VarAssetPath\relax
\global\let\VarCourseName\relax
\global\let\VarCourseNumber\relax
\global\let\VarCitationFile\relax
\global\let\VarDateD\relax
\global\let\VarDateM\relax
\global\let\VarDateY\relax
\global\let\VarDepartmentAbbrev\relax
\global\let\VarDepartmentName\relax
\global\let\VarInstitution\relax
\global\let\VarInstructor\relax
\global\let\VarStudent\relax
\global\let\VarSubtitle\relax
\global\let\VarTitle\relax

\def\AssetPath#1{\def\VarAssetPath{#1}}
\def\CourseName#1{\def\VarCourseName{#1}}
\def\CourseNumber#1{\def\VarCourseNumber{#1}}
\def\CitationFile#1{\def\VarCitationFile{#1}}
\def\DateD#1{\def\VarDateD{#1}}
\def\DateM#1{\def\VarDateM{#1}}
\def\DateY#1{\def\VarDateY{#1}}
\def\DepartmentAbbrev#1{\def\VarDepartmentAbbrev{#1}}
\def\DepartmentName#1{\def\VarDepartmentName{#1}}
\def\Institution#1{\def\VarInstitution{#1}}
\def\Instructor#1{\def\VarInstructor{#1}}
\def\Student#1{\def\VarStudent{#1}}
\def\Subtitle#1{\def\VarSubtitle{#1}}
\def\Title#1{\def\VarTitle{#1}}
%----------------------------- Report CUNY variables -------------------------


%----------------------------- Report CUNY document class parameters ---------
\RequirePackage{xkeyval}
% OPTION bibfile
% --> values = file name of your bibtex file
\define@key[ct]{ReportCUNY}{bibfile}[referencez]{\def\ReportCUNY@bibfile{#1} \def\VarCitationFile{#1}}
\setkeys[ct]{ReportCUNY}{bibfile=referencez}

\ProcessOptionsX[ct]<ReportCUNY>
%----------------------------- Report CUNY document class parameters ---------


%----------------------------- Setup biblatex-package options ------------------
\PassOptionsToPackage{%
backend=biber,
backref=true,
style=numeric-comp
}{biblatex}%
\RequirePackage{biblatex}%
%\addbibresource{\CitationFile}
%\bibliography{\CitationFile}%
%----------------------------- Setup biblatex-package options ------------------


%----------------------------- Setup fancy page boarder  -----------------------
\RequirePackage{background}
\usetikzlibrary{calc}
\SetBgAngle{0}
\SetBgOpacity{1}
\SetBgScale{1}
\SetBgColor{black}
\SetBgContents{%
\begin{tikzpicture}[overlay,remember picture]
\draw [line width=0.5mm,rounded corners=5mm,color=black]
    ($ (current page.north west) + (0.5in,-0.5in) $)
    rectangle
    ($ (current page.south east) + (-0.5in,0.5in) $);
\end{tikzpicture}
}
%----------------------------- Setup fancy page boarder  -----------------------


%----------------------------- Setup fancy page numbering ----------------------
\RequirePackage{lastpage}
\RequirePackage{fancyhdr}
\fancyhf{}
\fancyhead[C]{}
\fancyfoot[C]{\thepage\hspace{1mm}/\hspace{1mm}\pageref{LastPage}}
\renewcommand{\headrulewidth}{0pt}
\setlength{\footskip}{0.55in}
\pagestyle{fancy}
%----------------------------- Setup fancy page numbering ----------------------


%----------------------------- Setup hyperlink options -------------------------
\PassOptionsToPackage{%
dvipsnames,
svgnames,
hyperref}{xcolor}
\RequirePackage{xcolor}
\definecolor{LinkBlue}{RGB}{24,112,176}

\PassOptionsToPackage{%
pdftex,
pdfauthor          = {\VarStudent},%         - author  (PDF meta)
pdftitle           = {\VarTitle},%           - title   (PDF meta)
pdfsubject         = {\VarDepartmentName},%  - subject (PDF meta)
pdfdisplaydoctitle = true,
pdflang            = en-US,
pdfpagemode        = UseNone,
pdfpagelayout      = SinglePage,
pdfproducer        = ReportCUNY,
breaklinks         = true,%                - allow line break inside links
colorlinks         = true,%                - colorize links?
linkcolor          = black,
anchorcolor        = LinkBlue,
citecolor          = LinkBlue,
urlcolor           = LinkBlue
}{hyperref}
\RequirePackage{hyperref}
%----------------------------- Setup hyperlink options  -----------------------


%----------------------------- Augment paragraph intentation  -----------------
\PassOptionsToPackage{%
skip=.5\baselineskip%
}{parskip}
\RequirePackage{parskip}% http://ctan.org/pkg/parskip
%----------------------------- Augment paragraph intentation  -----------------


%----------------------------- Setup math modemodifications -------------------
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{amsthm}
\usepackage{lmodern}
\usepackage[T1]{fontenc}
\RequirePackage{nicefrac}
\RequirePackage{xspace}
\let\oldfrac\frac% Store \frac
\renewcommand{\frac}[2]{%
\mathchoice%
{\oldfrac{#1}{#2}}% display style
{\nicefrac{#1}{#2}}% text style
{\nicefrac{#1}{#2}}% script style
{\nicefrac{#1}{#2}}% script-script style
}
\newcommand{\Nats}{\ensuremath{\mathbb{N}}\xspace}
\newcommand{\Ints}{\ensuremath{\mathbb{Z}}\xspace}
\newcommand{\Qty}[1]{\textsf{\oldstylenums{\fontseries{m}\selectfont#1}}}
%----------------------------- Setup math mode modifications -------------------


%----------------------------- Other packages ----------------------------------
\usepackage{array}
\usepackage{calc}
\usepackage{changepage}
\usepackage{color}
\usepackage{crop}
\usepackage{enumitem}
\usepackage{etoolbox}
\usepackage{fancyhdr}
\usepackage{flushend}
\usepackage{graphicx}
\usepackage{lastpage}
\usepackage{latexsym}
\usepackage{lineno}
\usepackage{lipsum}
\usepackage{listings}
\usepackage{longtable}
\usepackage{lmodern}
\usepackage{stfloats}
\usepackage{setspace}
\usepackage{tabularx}
\usepackage{times}
\usepackage[normalem]{ulem}
\usepackage{wrapfig}
\usepackage{xifthen}
%----------------------------- Other packages ----------------------------------


%----------------------------------------------------------------------------------------
% Definition:  Title Page
%----------------------------------------------------------------------------------------
\newcommand{\BannerRule}{\rule{\linewidth}{0.5mm}}
\newcommand{\FormatCourse}{\VarDepartmentAbbrev~--~\VarCourseNumber}
\newcommand{\FormatDate}{\VarDateY\;--\;\VarDateM\;--\;\VarDateD}
\newcommand{\FormatEntry}[1]{\hspace{0.25in}#1}
\newcommand{\FormatLabel}[1]{\hspace{0.25in}\bfseries \textsc{#1}}
\newcommand{\FormatTitle}[1]{\bfseries\slshape#1\/}

\graphicspath{{\VarAssetPath}}

\newcommand{\ReportTitlePage}{%
\begin{titlepage}
\renewcommand{\familydefault}{\sfdefault}
\sffamily
\center% Center everything on the page
\vspace*{\fill}% Fill the "top page slack" with whitespace

%  ---  Section: Logo

\vspace*{-5mm}
\includegraphics[width=5cm]{CUNY-GC-Logo-Circle.png}\\[10mm] % Include a department/university logo - this will require the graphicx package

%  ---  Section: Title

\makeatletter
\begin{adjustwidth}{-0.25in}{-0.25in}
\center%
{%
\renewcommand{\parskip}{-8mm}
\BannerRule\\
{\Huge  \FormatTitle{\VarTitle}}\\
{\Large \FormatTitle{\VarSubtitle}}\\
\BannerRule%
}\\[16mm]

%  ---  Section: Atribution

{%
\renewcommand{\ULthickness}{0.5mm}
\uline{\makebox[4in][l]{\FormatEntry{\VarStudent}       }} \hfill \uline{\makebox[2.75in][l]{\FormatEntry{\VarInstructor}     }}\\[1mm]
       \makebox[2.75in][l]{\FormatLabel{Authors}          } \hfill        \makebox[2.75in][l]{\FormatLabel{Instructor}        }\\[10mm]
\uline{\makebox[2.75in][l]{\FormatEntry{\FormatDate}     }} \hfill \uline{\makebox[2.75in][l]{\FormatEntry{\FormatCourse}    }}\\[1mm]
       \makebox[2.5in][l]{\FormatLabel{Date}             } \hfill        \makebox[2.75in][l]{\FormatLabel{Course~\textnumero}}\\[10mm]
\uline{\makebox[2.75in][l]{\FormatEntry{\VarDepartmentName}}} \hfill \uline{\makebox[2.75in][l]{\FormatEntry{\VarCourseName}     }}\\[1mm]
       \makebox[2.75in][l]{\FormatLabel{Department}       } \hfill        \makebox[2.75in][l]{\FormatLabel{Course Name}       }
}
\end{adjustwidth}
\makeatother

\vspace*{\fill} % Fill the "bottom page slack" with whitespace
\end{titlepage}
}
%--------------------------------- Custom Title Page ---------------------------


%--------------------------------- Front-matter Pages --------------------------
\newcommand{\ReportReferences}{%
  {%
    \setstretch{1.1}
    \renewcommand{\bibfont}{\normalfont\small}
    \setlength{\biblabelsep}{0pt}
    \setlength{\bibitemsep}{0.5\baselineskip plus 0.5\baselineskip}% chktex 1
    \printbibliography%
  }
  \clearpage
}
%---------------------------------  Back-matter Pages --------------------------


%--------------------------------- Exported Commands ---------------------------
%\newcommand{\FrontMatter}{%
\AtBeginDocument{%
\renewcommand{\familydefault}{\sfdefault}
\ReportTitlePage%
\setcounter{page}{1}
\pagenumbering{arabic}% Always use Arabic numbering
\onehalfspacing%        Always use *1.5* spacing outside of Back-matter
\sffamily%
}

\AtEndDocument%
{%
\clearpage%
\singlespacing%         Always use *single* spacing within Back-matter
\ReportReferences%
}%
