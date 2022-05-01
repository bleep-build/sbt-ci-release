package com.geirsson

import bleep.RelPath
import bleep.internal.FileUtils
import bleep.logging.Logger
import com.geirsson.CiReleasePlugin._
import nosbt.librarymanagement.ScmInfo
import bleep.tasks.publishing._

import java.net.URL
import java.nio.file.{Files, Path}
import java.util.Base64
import scala.sys.process._
import scala.util.control.NonFatal

class CiReleasePlugin(val logger: Logger, val sonatype: Sonatype, val dynVer: DynVerPlugin, val pgp: PgpPlugin) {

  def ciRelease(files: Map[RelPath, Array[Byte]]): Unit =
    if (!isSecure) {
      sys.error("No access to secret variables, doing nothing")
    } else {
      logger.withContext(currentBranch).info(s"Running ci-release")
      setupGpg()
      if (!isTag) {
        if (isSnapshotVersion(dynVer.version)) {
          logger.info(s"No tag push, publishing SNAPSHOT")
          require(dynVer.dynverSonatypeSnapshots, "dynVer.config.dynverSonatypeSnapshots must be true")
          sys.error("publish snapshots not implemented yet")
//          publishSnapshots()
        } else {
          // Happens when a tag is pushed right after merge causing the master branch
          // job to pick up a non-SNAPSHOT version even if TRAVIS_TAG=false.
          sys.error("Snapshot releases must have -SNAPSHOT version number, doing nothing")
        }
      } else {
        logger.info("Tag push detected, publishing a stable release")
        sonatype.sonatypeClean()

        val synced = FileUtils.syncBytes(
          sonatype.sonatypeBundleDirectory,
          files,
          FileUtils.DeleteUnknowns.Yes(maxDepth = None),
          soft = false
        )
        val syncedExisting = synced.collect {
          case (path, FileUtils.Synced.Changed | FileUtils.Synced.Unchanged | FileUtils.Synced.New) => path
        }
        pgp.signedArtifacts(syncedExisting)

        sonatype.sonatypeBundleRelease()
      }
    }
}

object CiReleasePlugin {
  def isSecure: Boolean =
    System.getenv("TRAVIS_SECURE_ENV_VARS") == "true" ||
      System.getenv("BUILD_REASON") == "IndividualCI" ||
      System.getenv("PGP_SECRET") != null

  def isTag: Boolean =
    Option(System.getenv("TRAVIS_TAG")).exists(_.nonEmpty) ||
      Option(System.getenv("CIRCLE_TAG")).exists(_.nonEmpty) ||
      Option(System.getenv("CI_COMMIT_TAG")).exists(_.nonEmpty) ||
      Option(System.getenv("BUILD_SOURCEBRANCH"))
        .orElse(Option(System.getenv("GITHUB_REF")))
        .exists(_.startsWith("refs/tags"))

  def releaseTag: String =
    Option(System.getenv("TRAVIS_TAG"))
      .orElse(Option(System.getenv("BUILD_SOURCEBRANCH")))
      .orElse(Option(System.getenv("GITHUB_REF")))
      .orElse(Option(System.getenv("CIRCLE_TAG")))
      .orElse(Option(System.getenv("CI_COMMIT_TAG")))
      .getOrElse("<unknown>")

  def currentBranch: String =
    Option(System.getenv("TRAVIS_BRANCH"))
      .orElse(Option(System.getenv("BUILD_SOURCEBRANCH")))
      .orElse(Option(System.getenv("GITHUB_REF")))
      .orElse(Option(System.getenv("CIRCLE_BRANCH")))
      .orElse(Option(System.getenv("CI_COMMIT_BRANCH")))
      .getOrElse("<unknown>")

  def isAzure: Boolean =
    System.getenv("TF_BUILD") == "True"
  def isGithub: Boolean =
    System.getenv("GITHUB_ACTION") != null
  def isCircleCi: Boolean =
    System.getenv("CIRCLECI") == "true"
  def isGitlab: Boolean =
    System.getenv("GITLAB_CI") == "true"

  def setupGpg(): Unit = {
    val versionLine = List("gpg", "--version").!!.linesIterator.toList.head

    val TaggedVersion = """(\d{1,14})([\.\d{1,14}]*)((?:-\w+)*)""".r
    val gpgVersion: Long = versionLine.split(" ").last match {
      case TaggedVersion(m, _, _) => m.toLong
      case _                      => 0L
    }
    // https://dev.gnupg.org/T2313
    val importCommand =
      if (gpgVersion < 2L) "--import"
      else "--batch --import"
    val secret = sys.env("PGP_SECRET")
    if (isAzure) {
      // base64 encoded gpg secrets are too large for Azure variables but
      // they fit within the 4k limit when compressed.
      Files.write(Path.of("gpg.zip"), Base64.getDecoder.decode(secret))
      s"unzip gpg.zip".!
      s"gpg $importCommand gpg.key".!
    } else {
      (s"echo $secret" #| "base64 --decode" #| s"gpg $importCommand").!
    }
  }

  private def gitHubScmInfo(user: String, repo: String) =
    ScmInfo(
      new URL(s"https://github.com/$user/$repo"),
      s"scm:git:https://github.com/$user/$repo.git",
      Some(s"scm:git:git@github.com:$user/$repo.git")
    )

  def inferScmInfo: Option[ScmInfo] = {
    import scala.sys.process._
    val identifier = """([^\/]+?)"""
    val GitHubHttps = s"https://github.com/$identifier/$identifier(?:\\.git)?".r
    val GitHubGit = s"git://github.com:$identifier/$identifier(?:\\.git)?".r
    val GitHubSsh = s"git@github.com:$identifier/$identifier(?:\\.git)?".r
    try {
      val remote = List("git", "ls-remote", "--get-url", "origin").!!.trim()
      remote match {
        case GitHubHttps(user, repo) => Some(gitHubScmInfo(user, repo))
        case GitHubGit(user, repo)   => Some(gitHubScmInfo(user, repo))
        case GitHubSsh(user, repo)   => Some(gitHubScmInfo(user, repo))
        case _                       => None
      }
    } catch {
      case NonFatal(_) => None
    }
  }

  def isSnapshotVersion(v: String): Boolean =
    v.endsWith("-SNAPSHOT")
}
