package org.ucombinator.jade

import org.apache.maven.wagon.Wagon
import org.codehaus.plexus.DefaultContainerConfiguration
import org.codehaus.plexus.DefaultPlexusContainer
import org.codehaus.plexus.PlexusConstants
import org.apache.maven.index.Indexer
import org.apache.maven.index.updater.IndexUpdater

/*
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
*/

object Maven {
  def testArtifact(): Unit = {

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
