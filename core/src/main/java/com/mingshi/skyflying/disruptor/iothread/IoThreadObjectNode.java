package com.mingshi.skyflying.disruptor.iothread;

import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.Data;

/**
 * @ClassName FlowsDataWapper
 * @Description TODO
 * Author apple
 * Date 2021/11/14 12:32
 * @Version 1.0
 **/
@Data
public class IoThreadObjectNode {

  private ObjectNode data;

  public IoThreadObjectNode() {
  }

  public void setIoThreadObjectNode(ObjectNode data) {
    this.data = data;
  }
}
