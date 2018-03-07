package org.ucombinator.jade

import scala.collection.JavaConverters._

import java.io.File

import org.apache.maven.wagon.Wagon
import org.codehaus.plexus.DefaultContainerConfiguration
import org.codehaus.plexus.DefaultPlexusContainer
import org.codehaus.plexus.PlexusConstants
import org.apache.maven.index.Indexer
import org.apache.maven.index.updater.IndexUpdater
import org.apache.maven.index.context.IndexCreator
import org.apache.maven.index.updater.IndexUpdateRequest
import org.apache.maven.index.updater.IndexUpdateResult
import org.apache.maven.index.updater.ResourceFetcher
import org.apache.maven.index.updater.WagonHelper
import java.util

import org.apache.maven.wagon.observers.AbstractTransferListener
import org.apache.maven.wagon.events.TransferEvent

import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.MultiFields
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.util.Bits
import org.apache.maven.index.ArtifactInfo
import org.apache.maven.index.context.IndexUtils


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

    // Files where local cache is (if any) and Lucene Index should be located
    val centralLocalCache = new File("maven-cache/central-cache")
    val centralIndexDir = new File("maven-cache/central-index")

    // Creators we want to use (search for fields it defines)
    val indexers = new util.ArrayList[IndexCreator]
    indexers.add(plexusContainer.lookup(classOf[IndexCreator], "min"))
    indexers.add(plexusContainer.lookup(classOf[IndexCreator], "jarContent"))
    indexers.add(plexusContainer.lookup(classOf[IndexCreator], "maven-plugin"))

    // Create context for central repository index
    val centralContext = indexer.createIndexingContext("central-context", "central", centralLocalCache, centralIndexDir, "http://repo1.maven.org/maven2", null, true, true, indexers)

    // Update the index (incremental update will happen if this is not 1st run and files are not deleted)
    // This whole block below should not be executed on every app start, but rather controlled by some configuration
    // since this block will always emit at least one HTTP GET. Central indexes are updated once a week, but
    // other index sources might have different index publishing frequency.
    // Preferred frequency is once a week.
    if (true) {
      System.out.println("Updating Index...")
      System.out.println("This might take a while on first run, so please be patient!")
      // Create ResourceFetcher implementation to be used with IndexUpdateRequest
      // Here, we use Wagon based one as shorthand, but all we need is a ResourceFetcher implementation
      val listener = new AbstractTransferListener() {
        override def transferStarted(transferEvent: TransferEvent): Unit = {
          println("  Downloading " + transferEvent.getResource.getName)
        }

        override def transferProgress(transferEvent: TransferEvent, buffer: Array[Byte], length: Int): Unit
        =
        {
          //println("   - Progress: " + length)
        }
        override def transferCompleted(transferEvent: TransferEvent): Unit
        =
        {
          println("   - Done")
        }
      }
      val resourceFetcher = new WagonHelper.WagonFetcher(httpWagon, listener, null, null)
      val centralContextCurrentTimestamp = centralContext.getTimestamp
      val updateRequest = new IndexUpdateRequest(centralContext, resourceFetcher)
      val updateResult = indexUpdater.fetchAndUpdateIndex(updateRequest)
      if (updateResult.isFullUpdate) System.out.println("Full update happened!")
      else if (updateResult.getTimestamp == centralContextCurrentTimestamp) System.out.println("No update needed, index is up to date!")
      else System.out.println("Incremental update happened, change covered " + centralContextCurrentTimestamp + " - " + updateResult.getTimestamp + " period.")
      System.out.println()
    }

    System.out.println()
    System.out.println("Using index")
    System.out.println("===========")
    System.out.println()

    // ====
    // Case:
    // dump all the GAVs
    // NOTE: will not actually execute do this below, is too long to do (Central is HUGE), but is here as code
    // example
    if (true) {
      val searcher = centralContext.acquireIndexSearcher
      try {
        val ir = searcher.getIndexReader
        val liveDocs = MultiFields.getLiveDocs(ir)
        var i = 0
        while ( {
          i < ir.maxDoc
        }) {
          if (liveDocs == null || liveDocs.get(i)) {
            val doc = ir.document(i)
            val ai = IndexUtils.constructArtifactInfo(doc, centralContext)
            //System.out.println(ai.getGroupId + ":" + ai.getArtifactId + ":" + ai.getVersion + ":" + ai.getClassifier + " (sha1=" + ai.getSha1 + ")")
            println(ai)
            for (d <- doc.iterator().asScala) {
              println("field: " + d.fieldType() + ":" + d.name() + ":" + d.stringValue() + ":" + d)
            }
          }

          {
            i += 1; i - 1
          }
        }
      } finally centralContext.releaseIndexSearcher(searcher)
    }

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
