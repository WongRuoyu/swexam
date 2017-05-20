package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

import com.wong.model._
import net.liftmodules.FoBo

import scala.language.postfixOps
import scala.xml.NodeSeq
import net.liftweb.db.DBLogEntry

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot extends Logger {
  val logger = Logger(classOf[Boot])

  def boot {

    sys.props.put("h2.implicitRelativePath", "true")
    val vendor =
      new StandardDBVendor("org.h2.Driver", "jdbc:h2:swexam;AUTO_SERVER=TRUE", Empty, Empty)
    LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)
    DB.defineConnectionManager(util.DefaultConnectionIdentifier, vendor)


    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    /*        Schemifier.schemify(true, Schemifier.infoF _,
              User, KnowledgePoint, Question, UserQuestions, KnowledgePoint)*/

    Schemifier.schemify(true, Schemifier.infoF _,
      User, KnowledgePoint, ExamAssignment, Question, UserQuestions, QuestionKnowledgepts, Exam, ExamHistory)

    //Log数据库
    DB.addLogFunc {
      case (log, duration) => {
        this.debug("Total query time : %d ms".format(duration))
        log.allEntries.foreach {
          case DBLogEntry(stmt, duration) => this.debug("%s in %d ms".format(stmt, duration))
        }
      }
    }
    if (Props.devMode || Props.testMode) {
      LiftRules.liftRequest.append({
        case r if (r.path.partPath match {
          case "console" :: _ => true
          case _ => false
        }) => false
      })
    }

    // where to search snippet
    LiftRules.addToPackages("com.wong")


    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath("question" :: "show" :: key :: Nil, "", true, _), _, _) =>
        RewriteResponse("question" :: "question" :: Nil, Map("id" -> key))

      case RewriteRequest(ParsePath("question" :: "filter" :: key :: Nil, "", true, _), _, _) =>
        RewriteResponse("question" :: "questions" :: Nil, Map("kp_id" -> key))

      case RewriteRequest(ParsePath("public" :: "doexam" :: key :: Nil, "", true, _), _, _) =>
        RewriteResponse("public" :: "doexam" :: Nil, Map("exam_id" -> key))

      case RewriteRequest(ParsePath("public" :: "result" :: key :: Nil, "", true, _), _, _) =>
        RewriteResponse("public" :: "result" :: Nil, Map("examHist_id" -> key))
    }

    MapperRules.formatFormElement = (name: NodeSeq, form: NodeSeq) => <xml:group>
      <div class="control-group">
        <label class="control-label">
          {name}
        </label>
        <div class="controls">
          {form}
        </div>
      </div>
    </xml:group>


    SiteMap.enforceUniqueLinks = true

    def sitemapMutators = User.sitemapMutator

    //The SiteMap is built in the Site object bellow
    LiftRules.setSiteMapFunc(() => sitemapMutators(Site.sitemap))

    //Init the FoBo - Front-End Toolkit module,
    //see http://liftweb.net/lift_modules for more info
    FoBo.InitParam.JQuery = FoBo.JQuery1113
    FoBo.InitParam.ToolKit = FoBo.Bootstrap336
    FoBo.init()

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    LiftRules.noticesAutoFadeOut.default.set((notices: NoticeType.Value) => {
      notices match {
        case NoticeType.Notice => Full((8 seconds, 4 seconds))
        case _ => Empty
      }
    }
    )

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    //将默认的InMemFileParamHolde该写为OnDiskFileParamHolder。
    LiftRules.handleMimeFile = OnDiskFileParamHolder.apply

    /*    //配置开发模式下自动登录的用户
        def testUserLogin() {
          val testUser = User.find(By(User.firstName, "fcs2"))
          testUser.foreach(User.logUserIn(_))
        }*/

    //    User.autologinFunc = if (Props.devMode) Full(testUserLogin _) else Empty
  }

  object Site {

    val divider1 = Menu("divider1") / "divider1"

    val ddLabel1 = Menu.i("UserDDLabel") / "ddLabel1"
    val home = Menu.i("Homei") / "index" >> LocGroup("home")

    val questionsUpload = Menu(Loc("question_upload",
      "question" :: "upload" :: Nil,
      "UploadQuestions",
      LocGroup("admin")))
    val questionList = Menu(Loc("questionList",
      "question" :: "questions" :: Nil,
      "AllQuestions",
      LocGroup("admin")
    ))
    val questionDelete = Menu(Loc("questionDelete",
      "question" :: "delete" :: Nil,
      "Delete the Question",
      LocGroup("admin"), Hidden
    ))
    val questionEdit = Menu(Loc("questionEdit",
      "question" :: "edit" :: Nil,
      "Edit the Question",
      LocGroup("admin"), Hidden
    ))
    val question = Menu(Loc("questionDetail",
      "question" :: "question" :: Nil,
      "QuestionDetail",
      LocGroup("admin"), Hidden
    ))
    val questions = Menu(Loc("questions",
      "question" :: Nil,
      "Questions",
      LocGroup("exam"),
      FoBo.TBLocInfo.LinkTargetSelf, PlaceHolder
    ), question, questionsUpload, questionList, questionDelete, questionEdit)

    //exams 页面入口
    val examCreate = Menu(Loc("examCreate",
      "exam" :: "create" :: Nil,
      "Create A new Exam",
      LocGroup("admin")))
    val examList = Menu(Loc("examList",
      "exam" :: "exams" :: Nil,
      "All Exams",
      LocGroup("admin")))
    val examEdit = Menu(Loc("examEdit",
      "exam" :: "edit" :: Nil,
      "Edit Exams",
      LocGroup("admin"), Hidden))
    val examPreview = Menu(Loc("examPreview",
      "exam" :: "preview" :: Nil,
      "Preview An Exam",
      LocGroup("admin"), Hidden))
    val examAssign = Menu(Loc("examAssign",
      "exam" :: "assign" :: Nil,
      "Assign students to an Exam",
      LocGroup("admin"), Hidden))
    val examResult = Menu(Loc("examResult",
      "exam" :: "result" :: Nil,
      "Results of  an Exam",
      LocGroup("admin"), Hidden))
    val exams = Menu(Loc("exams",
      "exam" :: Nil,
      "All Exams",
      LocGroup("exam"),
      FoBo.TBLocInfo.LinkTargetSelf, PlaceHolder),
      examCreate, examList, examEdit, examPreview, examAssign, examResult)


    val loggedIn = If(() => User.loggedIn_?, () => RedirectResponse(User.loginPageURL))
    //
    val publicExam = Menu(Loc("publicExams",
      "public" :: "exams" :: Nil,
      "All Exams",
      LocGroup("public"), Hidden))
    val publicDoexam = Menu(Loc("publicDoexams",
      "public" :: "doexam" :: Nil,
      "Do Exams",
      LocGroup("public"), Hidden, loggedIn))
    val publicResult = Menu(Loc("publicResult",
      "public" :: "result" :: Nil,
      "Result",
      LocGroup("public"), Hidden, loggedIn))

    val publicPages = Menu(Loc("publicPages",
      "public" :: Nil,
      "public Pages",
      LocGroup("public"), Hidden), publicExam, publicDoexam, publicResult)

    val studentProfile = Menu(Loc("studentProfile",
      "student" :: "profile" :: Nil,
      "User Profile",
      LocGroup("student"), loggedIn))

    val studentsPages = Menu(Loc("studentsPages",
      "student" :: Nil,
      "Student",
      LocGroup("students"), FoBo.TBLocInfo.LinkTargetSelf, PlaceHolder),
      studentProfile)

    val userMenu = User.AddUserMenusHere
    val about = Menu(Loc("Static", Link(List("static"), true, "/static/about"), S.loc("about", scala.xml.Text("about")), LocGroup("about")))

    def sitemap = SiteMap(
      home,
      publicPages,
      studentsPages,
      exams,
      questions,
      ddLabel1 >> LocGroup("users") >> PlaceHolder submenus (
        divider1 >> FoBo.TBLocInfo.Divider >> userMenu
        )
    )
  }

}
