package bleep.plugin.cirelease

import bleep.logging.Logger
import bleep.packaging.Checksums
import bleep.plugin.cirelease.CiReleasePlugin._
import bleep.plugin.dynver.DynVerPlugin
import bleep.plugin.pgp.PgpPlugin
import bleep.plugin.sonatype.Sonatype
import bleep.{FileSync, RelPath}
import coursier.core.Info

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
      setupGpg(logger.processLogger("ci-release"))
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

        logger.warn(s"signing ${files.size} files")
        val signed = pgp.signedArtifacts(files)
        logger.warn(s"digesting ${signed.size} files")
        val digested = Checksums(signed, List(Checksums.Algorithm.Md5, Checksums.Algorithm.Sha1))

        logger.withContext(sonatype.sonatypeBundleDirectory).warn(s"writing bundle of ${digested.size} files")
        FileSync
          .syncBytes(
            sonatype.sonatypeBundleDirectory,
            digested,
            FileSync.DeleteUnknowns.Yes(maxDepth = None),
            soft = false
          )
          .log(logger, s"wrote sonatype bundle to ${sonatype.sonatypeBundleDirectory}")

        sonatype.sonatypeBundleRelease()
        ()
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

  def setupGpg(processLogger: ProcessLogger): Unit = {
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
      s"unzip gpg.zip".!!(processLogger)
      s"gpg $importCommand gpg.key".!!(processLogger)
      ()
    } else {
      (s"echo $secret" #| "base64 --decode" #| s"gpg $importCommand").!!(processLogger)
      ()
    }
  }

  private def gitHubScmInfo(user: String, repo: String) =
    Info.Scm(
      url = Some(s"https://github.com/$user/$repo"),
      connection = Some(s"scm:git:https://github.com/$user/$repo.git"),
      developerConnection = Some(s"scm:git:git@github.com:$user/$repo.git")
    )

  def inferScmInfo: Option[Info.Scm] = {
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
