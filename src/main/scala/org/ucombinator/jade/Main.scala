package org.ucombinator.jade

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.jar.{JarEntry, JarFile}

import scala.collection.JavaConverters._
import org.apache.commons.io.FileUtils
import org.apache.maven.artifact.handler.{ArtifactHandler, DefaultArtifactHandler}
import org.apache.maven.artifact.metadata.ArtifactMetadataSource
import org.apache.maven.artifact.{Artifact, DefaultArtifact}
import org.apache.maven.artifact.repository.{ArtifactRepository, ArtifactRepositoryPolicy, DefaultRepositoryRequest, MavenArtifactRepository}
import org.apache.maven.artifact.repository.layout.{ArtifactRepositoryLayout, DefaultRepositoryLayout}
import org.apache.maven.artifact.repository.metadata.{AbstractRepositoryMetadata, ArtifactRepositoryMetadata, Versioning}
import org.apache.maven.artifact.versioning.VersionRange
import org.apache.maven.project.artifact.DefaultMetadataSource
import org.codehaus.plexus.{ContainerConfiguration, DefaultPlexusContainer}
import org.eclipse.aether.metadata.DefaultMetadata
import org.rogach.scallop.ScallopConf

object Main extends App {
  val conf: Main = new Main(this.args)

  conf.subcommand match {
    case None => conf.errorMessageHandler("Missing subcommand")
    case Some(m: JadeSubcommand) => m.run()
    case Some(m) => conf.errorMessageHandler("Unknown subcommand: " + m)
  }
}

class Main(args: Seq[String]) extends ScallopConf(args = args) with JadeScallopConf {
  shortSubcommandsHelp(true)

  banner("Usage: jade [subcommand] [options]")
  addSubcommand(DecompileClass)
  addSubcommand(Decompile)
  addSubcommand(TestArtifact)
  verify()
}

object DecompileClass extends JadeSubcommand("decompile-class") {
  val className = trailArg[String]()

  override def run(): Unit = {
      DecompileOneClass.decompileOne(className())
  }
}

object Decompile extends JadeSubcommand("decompile") {
  val jarFile = trailArg[String]()
  val destinationFolder = trailArg[String]()

  override def run(): Unit = {
    // TODO: accept a directory that includes .class files
    val appName: String = jarFile()
    val destinationDirectoryName: String = destinationFolder()
    validityCheck(appName, destinationDirectoryName)

    val projectDirectory: Path = {
      val appBaseName = appName.split("/").last.stripSuffix(".jar")

      Paths.
        get(destinationDirectoryName, appBaseName).
        toAbsolutePath
    }

    createProjectDirectory(projectDirectory.toFile)
    val jar = new JarFile(appName)
    processJar(jar, projectDirectory)
  }


  private def validityCheck(appName: String, destinationDirectoryName: String)
  : Unit = {
    require(appName.endsWith(".jar"), "The first argument is NOT a .jar file!")

    val app = new File(appName)

    require(app.exists, s"The $appName file CANNOT be found!")
    require(app.canRead, s"The $appName file CANNOT be read!")

    val destinationDirectory = new File(destinationDirectoryName)

    require(destinationDirectory.exists, "The given destination (the second argument) does NOT exist!")
    require(destinationDirectory.isDirectory, "The given destination (the second argument) is NOT a directory!")
    require(destinationDirectory.canWrite, "The given destination directory (the second argument) is NOT writable!")
  }

  private def createProjectDirectory(directory: File)
  : Unit = {
    // TODO: Ask the user to decide what to do about this exist same name folder
    if (directory.exists) { FileUtils.deleteDirectory(directory) }
    assert(directory.mkdir(), s"CANNOT create folder $directory!")
  }

  private def processJar(jarFile: JarFile, projectDirectory: Path)
  : Unit = {
    val (directories, files) = jarFile.entries.asScala.partition(_.isDirectory)
    cloneJarFileDirectoryStructure(directories, projectDirectory)

    val (classFiles, nonClassFiles) = files.partition(_.getName.endsWith(".class"))
    cloneNonClassFilesInJar(jarFile, nonClassFiles, projectDirectory.toString)

    val decompiler: Decompiler = new Decompiler(jarFile, classFiles)
    decompiler.decompileToFiles()
  }

  private def cloneJarFileDirectoryStructure(directories: Iterator[JarEntry], outputDirectory: Path)
  : Unit =
    for (d <- directories) {
      // TODO: Create this annotation
      // @CanIgnoreReturnValue
      Files.createDirectory(Paths.get(outputDirectory.toString, d.getName))
    }

  private def cloneNonClassFilesInJar(jarFile: JarFile, nonClassFiles: Iterator[JarEntry], outputDirectoryName: String)
  : Unit =
      for (d <- nonClassFiles;
           path = Paths.get(outputDirectoryName, d.getName)) {
        Files.copy(jarFile.getInputStream(d), path, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      }
}

object TestArtifact extends JadeSubcommand("test-artifact") {
  val repoUrl = trailArg[String]()
  val groupId = trailArg[String]()
  val artifactId = trailArg[String]()

  def run(): Unit = {
    import org.apache.maven.wagon.Wagon
    import org.codehaus.plexus.DefaultContainerConfiguration
    import org.codehaus.plexus.DefaultPlexusContainer
    import org.codehaus.plexus.PlexusConstants
    import org.apache.maven.index.Indexer
    import org.apache.maven.index.updater.IndexUpdater

    // here we create Plexus container, the Maven default IoC container
    // Plexus falls outside of MI scope, just accept the fact that
    // MI is a Plexus component ;)
    // If needed more info, ask on Maven Users list or Plexus Users list
    // google is your friend!
    val config = new DefaultContainerConfiguration
    config.setClassPathScanning(PlexusConstants.SCANNING_INDEX)
    val plexusContainer = new DefaultPlexusContainer(config)

    // lookup the indexer components from plexus
    val indexer = plexusContainer.lookup(classOf[Indexer])
    val indexUpdater = plexusContainer.lookup(classOf[IndexUpdater])
    // lookup wagon used to remotely fetch index
    val httpWagon = plexusContainer.lookup(classOf[Wagon], "http")

  /*
    //org.apache.maven.artifact.repository.metadata.RepositoryMetadataManager
    //ContainerConfiguration
    //org.apache.maven.artifact.repository.metadata.DefaultRepositoryMetadataManager
    //org.apache.maven.repository.legacy.DefaultWagonManager
    val updateCheckManager = new org.apache.maven.repository.legacy.DefaultUpdateCheckManager()
    val container = new DefaultPlexusContainer()
    //container.addComponent(updateCheckManager, "org.apache.maven.repository.legacy.UpdateCheckManager")
    //val artifactMetadataSource = container.lookup(classOf[ArtifactMetadataSource])
    //org.codehaus.plexus.classworlds.strategy.SelfFirstStrategy
    //!!org.apache.maven.artifact.repository.metadata.io.xpp3.MetadataXpp3Reader

    //new DefaultRepositoryLayout()
    val snapshotPolicy = new ArtifactRepositoryPolicy(false, ArtifactRepositoryPolicy.UPDATE_POLICY_ALWAYS, ArtifactRepositoryPolicy.CHECKSUM_POLICY_FAIL)
    val releasesPolicy = new ArtifactRepositoryPolicy(false, ArtifactRepositoryPolicy.UPDATE_POLICY_ALWAYS, ArtifactRepositoryPolicy.CHECKSUM_POLICY_FAIL)
    val r = new MavenArtifactRepository(repoUrl(), repoUrl(), new DefaultRepositoryLayout(), snapshotPolicy, releasesPolicy)
    //r.pathOfRemoteRepositoryMetadata()
    val artifact = new DefaultArtifact(groupId(), artifactId(), "6.0.0", null, "jar", null, new DefaultArtifactHandler())
    val artifactMetadata = new ArtifactRepositoryMetadata(artifact)
    println("path: " + r.pathOfRemoteRepositoryMetadata(artifactMetadata))
    //Artifact.LATEST_VERSION
    //AbstractRepositoryMetadata
    //new DefaultMetadata
    //DefaultRepositoryRequest
    //Versioning
    //ArtifactMetadataSource
    //val metadataSource = new DefaultMetadataSource()
    //ArtifactRepository
    //val versions = artifactMetadataSource.retrieveAvailableVersions(artifact, null, List(r: ArtifactRepository).asJava)
    //val versions = r.findVersions(artifact)
    //r.find()
    //new DefaultArtifact(groupId: String, artifactId: String, versionRange: VersionRange, scope: String, `type`: String, classifier: String, artifactHandler: ArtifactHandler, optional: Boolean)
    //println(versions.asScala)
    */
  }
}
