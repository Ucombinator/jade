package org.ucombinator.jade

import java.net.URI

import scala.collection.mutable

object MavenRepositories {
  // Note that these are in order of number of indexed jars (largest first).
  // Note that we use `URI`s not `URL`s as `java.net.URL.equals()` involves network host resolution (!)
  val repositories = mutable.LinkedHashMap[String, URI](
    "Central" -> new URI("http://central.maven.org/maven2/"),
    "Sonatype Releases" -> new URI("https://oss.sonatype.org/content/repositories/releases/"),
    "Spring Plugins" -> new URI("http://repo.spring.io/plugins-release/"),
    "Spring Libs" -> new URI("http://repo.spring.io/libs-milestone/"),
    "Atlassian" -> new URI("https://maven.atlassian.com/content/repositories/atlassian-public/"),
    "JBoss Releases" -> new URI("https://repository.jboss.org/nexus/content/repositories/releases/"),
    "Nuxeo Releases" -> new URI("https://maven-eu.nuxeo.org/nexus/content/repositories/public-releases/"),
    "XWiki Releases" -> new URI("http://maven.xwiki.org/releases/"),
    "Apache Releases" -> new URI("https://repository.apache.org/content/repositories/releases/"),
    "Clojars" -> new URI("http://clojars.org/repo/"),
    "Redhat GA" -> new URI("https://maven.repository.redhat.com/ga/"),
    "Hortonworks" -> new URI("http://repo.hortonworks.com/content/repositories/releases/"),
    "Cloudera" -> new URI("https://repository.cloudera.com/content/repositories/releases/"),
    "Spinnaker" -> new URI("https://dl.bintray.com/spinnaker/spinnaker/"),
    "Alfresco Public" -> new URI("https://artifacts.alfresco.com/nexus/content/repositories/public/"),
    "Boundless" -> new URI("http://repo.boundlessgeo.com/main/"),
    "Cloudsoft" -> new URI("https://artifactory.cloudsoftcorp.com/artifactory/libs-release-local/"),
    "OpenNMS" -> new URI("http://repo.opennms.org/maven2/"),
    "WSO2" -> new URI("http://dist.wso2.org/maven2/"),
    "Java.net Releases" -> new URI("https://maven.java.net/content/repositories/releases/")
  )
}
