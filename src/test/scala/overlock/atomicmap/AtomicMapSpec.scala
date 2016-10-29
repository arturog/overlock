package overlock.atomicmap

import org.specs2.mutable._
import scala.collection.concurrent.{Map => ScalaConcurrentMap}
import java.util.concurrent.atomic._

abstract class AtomicMapSpec extends SpecificationWithJUnit {
  def createMap[A,B] : ScalaConcurrentMap[A,B]
  
  "AtomicMap" should {
    val map = createMap[String,Int]
    
    "be a full concurrentmap implementation" in {
      "getOrElseUpdate" in {
        map.getOrElseUpdate("blah", 1) must be_==(1)
        map.getOrElseUpdate("blah", 2) must be_==(1)
      }

      "replace(k,v)" in {
        map.replace("blah", 1) must beNone
        map.put("blah", 1)
        map.replace("blah", 2) must beSome(1)
      }

      "replace(k, o, n)" in {
        map.replace("blah", 2, 1) must be_==(false)
        map.put("blah", 1)
        println("map " + map)
        map.replace("blah", 1, 2) must be_==(true)
        map("blah") must be_==(2)
      }

      "remove" in {
        map.put("blah", 1)
        map.remove("blah", 2) must be_==(false)
        map.remove("blah", 1) must be_==(true)
        map.get("blah") must beNone
      }

      "putIfAbsent" in {
        map.putIfAbsent("blah", 1) must beNone
        map.putIfAbsent("blah", 2) must beSome(1)
        map.putIfAbsent("derp", 3) must beNone
      }

      "-=" in {
        map.put("herp", 1)
        map -= "herp"
        map.get("herp") must beNone
      }

      "+=" in {
        map += (("herp", 1))
        map.get("herp") must beSome(1)
      }

      "iterator" in {
        map.put("herp", 1)
        map.put("derp", 2)
        
        val otherMap = createMap[String,Int]

        for ((key,value) <- map) {
          otherMap.put(key,value)
        }
        
        otherMap.get("herp") must beSome(1)
        otherMap.get("derp") must beSome(2)
      }
      
      "get" in {
        map.put("herp", 5)
        map.get("herp") must beSome(5)
      }
      
      "return a new empty" in {
        map.put("herp", 1)
        val emptyMap = map.empty
        emptyMap.get("herp") must beNone
      }
    }
    
    "evaluate op only once" in {
      val counter = new AtomicInteger(0)
      
      val threads = for (i <- (0 to 5)) yield {
        new Thread {
          override def run {
            map.getOrElseUpdate("blah", {Thread.sleep(100); counter.incrementAndGet})
          }
        }
      }
      threads.foreach(_.start)
      threads.foreach(_.join)
      map("blah") must be_==(1)
      counter.get must be_==(1)
    }
  }
}

class AtomicNBHMSpec extends AtomicMapSpec {
  def createMap[A,B] = AtomicMap.atomicNBHM
}

class AtomicCSLMSpec extends AtomicMapSpec {
  def createMap[A,B] = AtomicMap.atomicCSLM
}

class AtomicCHMSpec extends AtomicMapSpec {
  def createMap[A,B] = AtomicMap.atomicCHM
}
