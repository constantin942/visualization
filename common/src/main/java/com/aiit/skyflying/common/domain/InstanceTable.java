package com.aiit.skyflying.common.domain;


import lombok.Data;


@Data
public class InstanceTable {

    String dbInstance;

    String msTableName;

    public InstanceTable(String dbInstance, String msTableName) {
        this.dbInstance=dbInstance;
        this.msTableName=msTableName;
    }
}
