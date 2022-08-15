package com.mingshi.skyflying.common.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.http.HttpStatus;

import java.io.Serializable;

/**
 * @author weilai
 * @email 352342845@qq.com
 * @date 2019-06-21 19:38
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CommonResponse<T> implements Serializable {

    private static final long serialVersionUID = -5332037387083810485L;

    /**
     * 错误码
     */
    private Integer code;

    /**
     * 提示信息
     */
    private String msg = "";

    /**
     * 具体数据
     */
    private T data;

    public static CommonResponse success(Object obj) {
        return new CommonResponse<Object>() {
            {
                this.setCode(HttpStatus.OK.value());
                this.setMsg("请求成功");
                this.setData(obj);
            }
        };
    }
    public static CommonResponse fail(String msg) {
        return new CommonResponse<Object>() {
            {
                this.setCode(1);
                this.setMsg(msg);
                this.setData(null);
            }
        };
    }


}
