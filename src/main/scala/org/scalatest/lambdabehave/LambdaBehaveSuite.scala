/*
 * Copyright 2001-2014 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.lambdabehave

import org.scalatest._
import com.insightfullogic.lambdabehave.{Suite => BehaveSuite, _}
import com.insightfullogic.lambdabehave.impl.reports.Specifiers
import java.util.stream.Collectors.toList
import collection.JavaConverters._
import org.scalatest.events._
import com.insightfullogic.lambdabehave.impl.reports.Result
import scala.reflect.NameTransformer
import org.scalatest.exceptions.PayloadField

class LambdaBehaveSuite(clazz: Class[_]) extends Suite { thisSuite =>

  val instance = clazz.newInstance
  val specifier = Specifiers.pop
  val children = specifier.completeBehaviours.collect(toList())
  val childrenMap = Map.empty ++ children.asScala.map(c => c.getDescription -> c)

  override def suiteName: String = specifier.getSuiteName

  override def suiteId: String = clazz.getName

  override def expectedTestCount(filter: Filter): Int = childrenMap.size

  override def testNames: Set[String] = childrenMap.keySet

  /*private def augmentedThreadName(currentName: String, suiteName: String): String = {
    val prefix =
      if (currentName.indexOf("ScalaTest-") == -1) currentName + "-ScalaTest"   // "pool-96-thread-1" => "pool-96-thread-1-ScalaTest-running-<suiteName>"
      else {                                                   // "ScalaTest-3-running-OldSpec" => "ScalaTest-3-running-<suiteName>"
      val regex = """(.*?)-running-.*""".r                   // "ScalaTest-3" => "ScalaTest-3-running-<suiteName"
      val pMatcher = regex.pattern.matcher(currentName)
        val matches = pMatcher.matches()
        if (matches) pMatcher.group(1) else currentName
      }
    prefix + "-running-" + suiteName
  }

  // Wrap any non-DispatchReporter, non-CatchReporter in a CatchReporter,
  // so that exceptions are caught and transformed
  // into error messages on the standard error stream.
  private def wrapReporterIfNecessary(theSuite: Suite, reporter: Reporter): Reporter = reporter match {
    case cr: CatchReporter => cr
    case _ => theSuite.createCatchReporter(reporter)
  }

  override def run(testName: Option[String], args: Args): Status = {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val originalThreadName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName(augmentedThreadName(originalThreadName, suiteName))

      val stopRequested = stopper
      val report = wrapReporterIfNecessary(thisSuite, reporter)
      val newArgs = args.copy(reporter = report)

      val testsStatus = runTests(testName, newArgs)

      if (stopRequested()) {
        val rawString = Resources("executeStopping")
        report(InfoProvided(tracker.nextOrdinal(), rawString, Some(NameInfo(thisSuite.suiteName, thisSuite.suiteId, Some(clazz.getClass.getName), testName))))
      }
      testsStatus
    }
    finally Thread.currentThread.setName(originalThreadName)
  }*/

  private def reportTestStarting(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, rerunnable: Option[String], location: Option[Location]) {
    report(TestStarting(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, Some(MotionToSuppress),
      location, rerunnable))
  }

  private def reportTestSucceeded(theSuite: Suite, report: Reporter, tracker: Tracker, testName: String, testText: String, recordedEvents: collection.immutable.IndexedSeq[RecordableEvent], duration: Long, formatter: Formatter, rerunnable: Option[String], location: Option[Location]) {
    report(TestSucceeded(tracker.nextOrdinal(), theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName, testText, recordedEvents, Some(duration), Some(formatter),
      location, rerunnable))
  }

  def getEscapedIndentedTextForTest(testText: String, level: Int, includeIcon: Boolean) = {
    val decodedTestText = NameTransformer.decode(testText)
    val escapedTestText =
      if (decodedTestText.startsWith("test: "))
        decodedTestText.drop(6)
      else
        decodedTestText
    val formattedText =
      if (includeIcon) {
        val testSucceededIcon = Resources("testSucceededIconChar")
        ("  " * (if (level == 0) 0 else (level - 1))) + Resources("iconPlusShortName", testSucceededIcon, escapedTestText)
      }
      else {
        ("  " * level) + escapedTestText
      }
    IndentedText(formattedText, decodedTestText, level)
  }

  def getIndentedTextForInfo(message: String, level: Int, includeIcon: Boolean, infoIsInsideATest: Boolean) = {
    val formattedText =
      if (includeIcon) {
        val infoProvidedIcon = Resources("infoProvidedIconChar")
        //
        // Inside a test, you want level 1 to stay 1
        // [scalatest] - outermost test (5 milliseconds)
        // [scalatest]   + in outermost test
        //
        // But outside a test, level 1 should be transformed to 0
        // [scalatest] Apple
        // [scalatest] + in Apple
        //
        val indentationLevel =
          level match {
            case 0 => 0
            case 1 if infoIsInsideATest => 1
            case _ => level - 1
          }
        ("  " * indentationLevel) + Resources("iconPlusShortName", infoProvidedIcon, message)
        // ("  " * (if (level <= 1) level else (level - 1))) + Resources("iconPlusShortName", infoProvidedIcon, message)
      }
      else {
        ("  " * level) + message
      }
    IndentedText(formattedText, message, level)
  }

  def createInfoProvided(theSuite: Suite,
                         report: Reporter,
                         tracker: Tracker,
                         testName: Option[String],
                         message: String,
                         payload: Option[Any],
                         level: Int,
                         location: Option[Location],
                         includeNameInfo: Boolean,
                         includeIcon: Boolean = true) = {
    InfoProvided(
      tracker.nextOrdinal(),
      message,
      if (includeNameInfo)
        Some(NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName))
      else
        None,
      None,
      Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined)),
      location,
      payload
    )
  }

  def createMarkupProvided(
                            theSuite: Suite,
                            report: Reporter,
                            tracker: Tracker,
                            testName: Option[String],
                            message: String,
                            level: Int,
                            location: Option[Location],
                            includeNameInfo: Boolean,
                            includeIcon: Boolean = true
                            ) = {
    MarkupProvided(
      tracker.nextOrdinal(),
      message,
      if (includeNameInfo)
        Some(NameInfo(theSuite.suiteName, theSuite.suiteId, Some(theSuite.getClass.getName), testName))
      else
        None,
      Some(getIndentedTextForInfo(message, level, includeIcon, testName.isDefined)),
      location
    )
  }

  override def tags: Map[String, Set[String]] = Map.empty

  override protected def runTest(testName: String, args: Args): Status = {

    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")

    import args._

    val testStartTime = System.currentTimeMillis
    reportTestStarting(this, reporter, tracker, testName, testName, rerunner, Some(LineInFile(88, "TODO: How to get the location??")))

    val formatter = getEscapedIndentedTextForTest(testName, 1, true)

    val messageRecorderForThisTest = new MessageRecorder(reporter)
    val informerForThisTest =
      MessageRecordingInformer(
        messageRecorderForThisTest,
        (message, payload, isConstructingThread, testWasPending, testWasCanceled, location) => createInfoProvided(thisSuite, reporter, tracker, Some(testName), message, payload, 2, location, isConstructingThread, true)
      )

    val documenterForThisTest =
      MessageRecordingDocumenter(
        messageRecorderForThisTest,
        (message, _, isConstructingThread, testWasPending, testWasCanceled, location) => createMarkupProvided(thisSuite, reporter, tracker, Some(testName), message, 2, location, isConstructingThread) // TODO: Need a test that fails because testWasCanceleed isn't being passed
      )

    childrenMap.get(testName) match {
      case Some(child) =>
        val report = child.checkCompleteBehaviour()
        report.getResult match {
          case Result.SUCCESS =>
            val duration = System.currentTimeMillis - testStartTime
            reportTestSucceeded(this, reporter, tracker, testName, testName, messageRecorderForThisTest.recordedEvents(false, false), duration, formatter, rerunner, Some(LineInFile(88, "TODO: How to get the location??")))
            SucceededStatus
          case Result.FAILURE | Result.ERROR =>
            val duration = System.currentTimeMillis - testStartTime
            reporter(TestFailed(tracker.nextOrdinal(), report.getMessage, thisSuite.suiteName, thisSuite.suiteId, Some(thisSuite.getClass.getName), testName, testName, messageRecorderForThisTest.recordedEvents(false, false), None, Some(duration), Some(formatter), Some(LineInFile(88, "TODO: How to get the location??")), thisSuite.rerunner, None))
            FailedStatus
        }
      case None =>
        throw new IllegalArgumentException("Test '" + testName + "' not found.")
    }



    //val (stopRequested, report, method, testStartTime) =
      //getSuiteRunTestGoodies(thisSuite, stopper, reporter, testName)



  }

}