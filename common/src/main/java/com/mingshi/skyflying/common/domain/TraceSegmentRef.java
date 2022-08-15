/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package com.mingshi.skyflying.common.domain;

import lombok.Getter;
import org.apache.skywalking.apm.network.language.agent.v3.RefType;
import org.apache.skywalking.apm.network.language.agent.v3.SegmentReference;

@Getter
public class TraceSegmentRef {
    private SegmentRefType type;
    private String traceId;
    private String traceSegmentId; // 是当前Segment的Parent的traceSegmentId；2022-04-22 16:11:18
    private int spanId;
    private String parentService; // 假设商城应用 Mall 调用了 Order 这个订单应用，那么对于 Order 应用来说，parentService就是 Mall；2022-04-22 16:12:46
    private String parentServiceInstance; // 一个应用可能部署了多个实例，这个parentServiceInstance就记录了 parentService 的一个具体实例；2022-04-22 16:14:18
    private String parentEndpoint; // 进入 parentService 的那个请求；
    private String addressUsedAtClient;

    /**
     * <B>方法名称：transform</B>
     * <B>概要说明：把 java 对象序列化为Protobuf对象 </B>
     * @Author zm
     * @Date 2022年04月22日 16:04:45
     * @Param []
     * @return org.apache.skywalking.apm.network.language.agent.v3.SegmentReference
     **/
    public SegmentReference transform() {
        SegmentReference.Builder refBuilder = SegmentReference.newBuilder();
        if (SegmentRefType.CROSS_PROCESS.equals(type)) {
            refBuilder.setRefType(RefType.CrossProcess);
        } else {
            refBuilder.setRefType(RefType.CrossThread);
        }
        refBuilder.setTraceId(traceId);
        refBuilder.setParentTraceSegmentId(traceSegmentId);
        refBuilder.setParentSpanId(spanId);
        refBuilder.setParentService(parentService);
        refBuilder.setParentServiceInstance(parentServiceInstance);
        refBuilder.setParentEndpoint(parentEndpoint);
        if (addressUsedAtClient != null) {
            refBuilder.setNetworkAddressUsedAtPeer(addressUsedAtClient);
        }

        return refBuilder.build();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
          return true;
        }
        if (o == null || getClass() != o.getClass()) {
          return false;
        }

        TraceSegmentRef ref = (TraceSegmentRef) o;

        if (spanId != ref.spanId) {
          return false;
        }
        return traceSegmentId.equals(ref.traceSegmentId);
    }

    @Override
    public int hashCode() {
        int result = traceSegmentId.hashCode();
        result = 31 * result + spanId;
        return result;
    }

    public enum SegmentRefType {
        CROSS_PROCESS, CROSS_THREAD
    }
}
