(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[72],{29455:function(q,h,n){"use strict";n.r(h),n.d(h,{default:function(){return A}});var U=n(13254),j=n(14277),X=n(98858),y=n(4914),I=n(2824),b=n(67294),w=n(3182),D=n(94043),x=n.n(D),z=n(25377);function E(){return S.apply(this,arguments)}function S(){return S=(0,w.Z)(x().mark(function i(){return x().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,z.WY)("/api/skyflying/getAllSegments",{method:"GET",timeout:6e4,headers:{"Content-Type":"application/json"}}));case 1:case"end":return t.stop()}},i)})),S.apply(this,arguments)}var W=n(87593),C=n(37636),K=n(71153),P=n(60331),Q=n(54029),M=n(79166),J=n(99270),f=n(57895),B=n(69610),L=n(54941),_=function(){function i(){(0,B.Z)(this,i)}return(0,L.Z)(i,null,[{key:"timeStampToTime",value:function(t){var a=new Date(parseInt(t));return a.toJSON().substring(0,10).replace("T","")}},{key:"getLocalTime",value:function(t,a){var l=t||new Date,o,m={"Y+":l.getFullYear().toString(),"m+":this.twoCompletion(l.getMonth()+1),"d+":this.twoCompletion(l.getDate()),"H+":this.twoCompletion(l.getHours()),"M+":this.twoCompletion(l.getMinutes()),"S+":this.twoCompletion(l.getSeconds())},c="";return Object.keys(m).forEach(function(u){o=new RegExp("("+u+")").exec(a),o&&(c=a.replace(o[1],o[1].length===1?m[u]:m[u].padStart(o[1].length,"0")))}),c}}]),i}();_.arrayDeDuplication=function(i){return Array.from(new Set(i))},_.twoCompletion=function(i){return i.toString().padStart(2,"0")};var e=n(85893),Z=[{title:"peer",align:"center",ellipsis:!0,dataIndex:"peer",tip:"",fixed:"left",render:function(s,t){return(0,e.jsx)(M.Z,{status:"processing",text:t.peer})}},{title:"component",align:"center",dataIndex:"component",tip:""},{title:"endpointName",align:"center",dataIndex:"endpointName",tip:""},{title:"serviceInstanceName",align:"center",dataIndex:"serviceInstanceName",tip:""},{title:"startTime",align:"center",dataIndex:"startTime",tip:"",render:function(s,t){return(0,e.jsx)("div",{children:_.timeStampToTime(t.startTime)})}},{title:"endTime",align:"center",dataIndex:"endTime",tip:"",render:function(s,t){return(0,e.jsx)("div",{children:_.timeStampToTime(t.endTime)})}},{title:"db.type",align:"center",dataIndex:"db.type",render:function(s,t){var a;return(0,e.jsx)("div",{children:(a=t.tags[0])===null||a===void 0?void 0:a.value})}},{title:"db.instance",dataIndex:"db.instance",align:"center",render:function(s,t){var a;return(0,e.jsx)("div",{children:(a=t.tags[1])===null||a===void 0?void 0:a.value})}},{title:"db.statement",align:"center",dataIndex:"db.statement",width:300,render:function(s,t){var a;return(0,e.jsx)("div",{children:((a=t.tags[2])===null||a===void 0?void 0:a.value)||"-"})}}],G=function(s){var t=(0,b.useState)(0),a=(0,I.Z)(t,2),l=a[0],o=a[1],m=(0,b.useState)([]),c=(0,I.Z)(m,2),u=c[0],T=c[1],O=function(){return JSON.parse(s.body[0].reorganizingSpans).filter(function(r){return r.spanId!==void 0}).map(function(r){var p,d,g;return r["db.type"]=(p=r.tags[0])===null||p===void 0?void 0:p.value,r["db.instance"]=(d=r.tags[1])===null||d===void 0?void 0:d.value,r["db.statement"]=(g=r.tags[2])===null||g===void 0?void 0:g.value,r})},V=function(r,p){T([]),o(p),r.length>1&&T(r.filter(function(d){return d.spanId!==void 0}).map(function(d){var g,N,k;return d["db.type"]=(g=d.tags[0])===null||g===void 0?void 0:g.value,d["db.instance"]=(N=d.tags[1])===null||N===void 0?void 0:N.value,d["db.statement"]=(k=d.tags[2])===null||k===void 0?void 0:k.value,d}))};return(0,b.useEffect)(function(){T(s.body.length===1?O():[]),console.log(u)},[]),(0,e.jsx)("div",{className:"detail-table",children:(0,e.jsxs)(f.ZP,{split:"vertical",children:[s.body.length>1&&(0,e.jsx)(f.ZP,{colSpan:"30%",children:(0,e.jsx)(C.Z,{children:s.body.map(function(v,r){var p=JSON.parse(v.reorganizingSpans);return(0,e.jsx)(C.Z.Item,{color:p.length>1?"#00CCFF":"green",className:"pointer pb-10",children:(0,e.jsxs)("div",{onClick:function(){V(p,r)},children:[(0,e.jsx)("p",{className:l===r?"is-active":"",children:p[0].url}),(0,e.jsx)(P.Z,{color:"#3b5999",style:{fontSize:"12px",padding:"2px 3px",lineHeight:"1"},children:v.operationName}),(0,e.jsx)("span",{style:{color:"#C0C4CC",fontSize:"12px"},children:v.requestStartTime})]})},r)})})}),(0,e.jsx)(f.ZP,{headerBordered:!0,style:{overflowX:"auto"},children:u.length>0?(0,e.jsx)(J.ZP,{style:{overflowX:"auto"},columns:Z,options:!1,size:"small",dataSource:u,rowKey:"spanId",scroll:{x:"100px"},search:!1,pagination:!1}):(0,e.jsx)(j.Z,{description:"\u6682\u65E0\u64CD\u4F5C\u6570\u636E"})})]})})},H=G,$=n(6251),F=n.n($),R=[{header:{userName:"admin",url:"http://localhost:7070/user/login",requestStartTime:"2022-05-17 10:35:06"},body:[{reorganizingSpans:'[{"url":"http://localhost:7070/user/login"}, {"spanId":2,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754906310,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906313,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n     \\n    id, is_delete, gmt_create, gmt_modified, creator, modifier, status, user_name, password,\\n    salt, phone, name, email\\n   \\n    from sys_operator\\n    where user_name=?"}]}, {"spanId":5,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754906314,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906316,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n         \\n    id, is_delete, gmt_create, gmt_modified, user_name, password_error_count, description\\n   \\n        from aiit_user_login_statistics\\n        where is_delete=0 AND user_name=?"}]}, {"spanId":7,"component":"Lettuce","serviceCode":"demo-application-springboot","peer":"10.0.107.19:6379","endpointName":"Lettuce/SETEX","startTime":1652754906320,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906324,"parentSpanId":0,"tags":[{"key":"db.type","value":"Redis"},{"key":"db.statement","value":"SETEX"}]}, {"spanId":8,"component":"SpringRestTemplate","serviceCode":"demo-application-springboot","peer":"localhost:8080","endpointName":"/demo1/get","startTime":1652754906334,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906411,"parentSpanId":0,"tags":[{"key":"url","value":"http://localhost:8080/demo1/get"},{"key":"http.method","value":"GET"}]}]',operationName:"POST:/user/login",id:1,requestStartTime:"2022-05-17 10:35:06"}]},{header:{userName:"admin",url:"http://localhost:7070/firewall/sysmenu",requestStartTime:"2022-05-17 10:35:06"},body:[{reorganizingSpans:`[{"url":"http://localhost:7070/firewall/sysmenu"}, {"spanId":2,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754906418,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906423,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"insert into aiit_operate_log\\n         ( gmt_create,\\n            \\n            \\n            \\n            \\n                login_ip,\\n            \\n            \\n                method_name,\\n            \\n            \\n                request_url,\\n            \\n            \\n            \\n            \\n                order_id ) \\n         values ( ?,\\n            \\n            \\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n            \\n            \\n                ? )"}]}, {"spanId":4,"component":"Lettuce","serviceCode":"demo-application-springboot","peer":"10.0.107.19:6379","endpointName":"Lettuce/GET","startTime":1652754906424,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906427,"parentSpanId":0,"tags":[{"key":"db.type","value":"Redis"},{"key":"db.statement","value":"GET"}]}, {"spanId":6,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754906428,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906431,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n     \\n    id, is_delete, gmt_create, gmt_modified, creator, modifier, status, user_name, password,\\n    salt, phone, name, email\\n   \\n    from sys_operator\\n    where user_name=?"}]}, {"spanId":9,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754906432,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906436,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n         \\n    id,\\n    date_format(gmt_create, '%Y-%m-%d %H:%i:%s') as gmt_create,\\n    date_format(gmt_modified, '%Y-%m-%d %H:%i:%s') as gmt_modified,\\n    user_name,\\n    login_ip,\\n    method_name,\\n    request_url,\\n    request_params,\\n    response_params,\\n    order_id\\n   \\n        from aiit_operate_log\\n        where is_delete=0 and  order_id=?"}]}, {"spanId":12,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754906437,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754906442,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"update aiit_operate_log\\n         SET gmt_create=?,\\n            \\n            \\n                gmt_modified=?,\\n            \\n            \\n                user_name=?,\\n            \\n            \\n                login_ip=?,\\n            \\n            \\n                method_name=?,\\n            \\n            \\n                request_url=?,\\n            \\n            \\n                request_params=?,\\n            \\n            \\n                response_params=?,\\n            \\n            \\n                order_id=? \\n        where id=?"}]}]`,operationName:"POST:/firewall/sysmenu",id:2,requestStartTime:"2022-05-17 10:35:06"}]},{header:{userName:"admin",url:"http://localhost:7070/user/isLogin",requestStartTime:"2022-05-17 10:35:07"},body:[{reorganizingSpans:'[{"url":"http://localhost:7070/user/isLogin"}, {"spanId":1,"component":"Lettuce","serviceCode":"demo-application-springboot","peer":"10.0.107.19:6379","endpointName":"Lettuce/GET","startTime":1652754907558,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907570,"parentSpanId":0,"tags":[{"key":"db.type","value":"Redis"},{"key":"db.statement","value":"GET"}]}]',operationName:"POST:/user/isLogin",id:3,requestStartTime:"2022-05-17 10:35:07"}]},{header:{userName:"admin",url:"http://localhost:7070/firewall/getFirewall",requestStartTime:"2022-05-17 10:35:07"},body:[{reorganizingSpans:'[{"url":"http://localhost:7070/firewall/getFirewall"}, {"spanId":2,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907660,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907664,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select firewall_exchange_id\\n    from ms_distributed_firewall_rules\\n    group by firewall_exchange_id"}]}, {"spanId":5,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907665,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907670,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"SELECT\\n    firewall_id\\n    from flows_firewall_exchange_match\\n    where id IN\\n     (  \\n      ?\\n     )"}]}, {"spanId":8,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907671,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907673,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"SELECT\\n        firewall_name as firewallName,firewall_ip as firewallIp\\n        from flows_firewalls\\n        where id IN\\n         (  \\n            ?\\n         )"}]}]',operationName:"GET:/firewall/getFirewall",id:4,requestStartTime:"2022-05-17 10:35:07"}]},{header:{userName:"admin",url:"http://localhost:7070/firewall/getVisitedDesinationIpCount",requestStartTime:"2022-05-17 10:35:07"},body:[{reorganizingSpans:`[{"url":"http://localhost:7070/firewall/getVisitedDesinationIpCount"}, {"spanId":2,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907674,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907680,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"insert into aiit_operate_log\\n         ( gmt_create,\\n            \\n            \\n            \\n            \\n                login_ip,\\n            \\n            \\n                method_name,\\n            \\n            \\n                request_url,\\n            \\n            \\n            \\n            \\n                order_id ) \\n         values ( ?,\\n            \\n            \\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n            \\n            \\n                ? )"}]}, {"spanId":4,"component":"Lettuce","serviceCode":"demo-application-springboot","peer":"10.0.107.19:6379","endpointName":"Lettuce/GET","startTime":1652754907680,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907684,"parentSpanId":0,"tags":[{"key":"db.type","value":"Redis"},{"key":"db.statement","value":"GET"}]}, {"spanId":6,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907686,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907697,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n     \\n    id\\n    , r_key, r_value, source_ip_group, firewall_exchange_match_id, gmt_create, gmt_update\\n   \\n    from destinationIp_statistics\\n    order by r_value desc limit ?"}]}, {"spanId":9,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907701,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907703,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n         \\n    id,\\n    date_format(gmt_create, '%Y-%m-%d %H:%i:%s') as gmt_create,\\n    date_format(gmt_modified, '%Y-%m-%d %H:%i:%s') as gmt_modified,\\n    user_name,\\n    login_ip,\\n    method_name,\\n    request_url,\\n    request_params,\\n    response_params,\\n    order_id\\n   \\n        from aiit_operate_log\\n        where is_delete=0 and  order_id=?"}]}, {"spanId":12,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907708,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907713,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"update aiit_operate_log\\n         SET gmt_create=?,\\n            \\n            \\n                gmt_modified=?,\\n            \\n            \\n                user_name=?,\\n            \\n            \\n                login_ip=?,\\n            \\n            \\n                method_name=?,\\n            \\n            \\n                request_url=?,\\n            \\n            \\n                request_params=?,\\n            \\n            \\n                response_params=?,\\n            \\n            \\n                order_id=? \\n        where id=?"}]}]`,operationName:"GET:/firewall/getVisitedDesinationIpCount",id:5,requestStartTime:"2022-05-17 10:35:07"}]},{header:{userName:"admin",url:"http://localhost:7070/firewall/getAllFlowsStrategyMatchCount",requestStartTime:"2022-05-17 10:35:07"},body:[{reorganizingSpans:`[{"url":"http://localhost:7070/firewall/getAllFlowsStrategyMatchCount"}, {"spanId":2,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907664,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907673,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"insert into aiit_operate_log\\n         ( gmt_create,\\n            \\n            \\n            \\n            \\n                login_ip,\\n            \\n            \\n                method_name,\\n            \\n            \\n                request_url,\\n            \\n            \\n            \\n            \\n                order_id ) \\n         values ( ?,\\n            \\n            \\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n            \\n            \\n                ? )"}]}, {"spanId":4,"component":"Lettuce","serviceCode":"demo-application-springboot","peer":"10.0.107.19:6379","endpointName":"Lettuce/GET","startTime":1652754907673,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907678,"parentSpanId":0,"tags":[{"key":"db.type","value":"Redis"},{"key":"db.statement","value":"GET"}]}, {"spanId":6,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907680,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907682,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n        id as firewallId\\n        from flows_firewalls\\n        order by firewallId asc"}]}, {"spanId":9,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907683,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907689,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select count(*)\\n        from flows_firewalls"}]}, {"spanId":12,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907690,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907692,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select id\\n    from flows_firewall_exchange_match\\n    where firewall_id IN\\n     (  \\n      (?)\\n     , \\n      (?)\\n     )"}]}, {"spanId":15,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907692,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907694,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"SELECT\\n    sum( r_value )\\n    FROM\\n    all_flows_statistics\\n    WHERE\\n    firewall_exchange_match_id IN\\n     (  \\n      (?)\\n     )"}]}, {"spanId":18,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907695,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907698,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select count(id)\\n    from ms_distributed_firewall_rules\\n    where is_delete = 0 and\\n    firewall_exchange_id IN\\n     (  \\n      (?)\\n     )"}]}, {"spanId":21,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907699,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907702,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select count(id)\\n    from all_flows_strategy_statistics\\n    where firewall_exchange_match_id in\\n     (  \\n      (?)\\n     )"}]}, {"spanId":24,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907703,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907705,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n         \\n    id,\\n    date_format(gmt_create, '%Y-%m-%d %H:%i:%s') as gmt_create,\\n    date_format(gmt_modified, '%Y-%m-%d %H:%i:%s') as gmt_modified,\\n    user_name,\\n    login_ip,\\n    method_name,\\n    request_url,\\n    request_params,\\n    response_params,\\n    order_id\\n   \\n        from aiit_operate_log\\n        where is_delete=0 and  order_id=?"}]}, {"spanId":27,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907707,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907713,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"update aiit_operate_log\\n         SET gmt_create=?,\\n            \\n            \\n                gmt_modified=?,\\n            \\n            \\n                user_name=?,\\n            \\n            \\n                login_ip=?,\\n            \\n            \\n                method_name=?,\\n            \\n            \\n                request_url=?,\\n            \\n            \\n                request_params=?,\\n            \\n            \\n                response_params=?,\\n            \\n            \\n                order_id=? \\n        where id=?"}]}]`,operationName:"GET:/firewall/getAllFlowsStrategyMatchCount",id:6,requestStartTime:"2022-05-17 10:35:07"}]},{header:{userName:"admin",url:"http://localhost:7070/firewall/getVisitSourceIpCount",requestStartTime:"2022-05-17 10:35:07"},body:[{reorganizingSpans:`[{"url":"http://localhost:7070/firewall/getVisitSourceIpCount"}, {"spanId":2,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907766,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907772,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"insert into aiit_operate_log\\n         ( gmt_create,\\n            \\n            \\n            \\n            \\n                login_ip,\\n            \\n            \\n                method_name,\\n            \\n            \\n                request_url,\\n            \\n            \\n            \\n            \\n                order_id ) \\n         values ( ?,\\n            \\n            \\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n            \\n            \\n                ? )"}]}, {"spanId":4,"component":"Lettuce","serviceCode":"demo-application-springboot","peer":"10.0.107.19:6379","endpointName":"Lettuce/GET","startTime":1652754907773,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907779,"parentSpanId":0,"tags":[{"key":"db.type","value":"Redis"},{"key":"db.statement","value":"GET"}]}, {"spanId":6,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907781,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907831,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n     \\n    id\\n    , r_key, r_value, source_ip_group, firewall_exchange_match_id, gmt_create, gmt_update\\n   \\n    from sourceIp_statistics\\n    order by r_value desc\\n    limit ?"}]}, {"spanId":9,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907834,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907836,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n         \\n    id,\\n    date_format(gmt_create, '%Y-%m-%d %H:%i:%s') as gmt_create,\\n    date_format(gmt_modified, '%Y-%m-%d %H:%i:%s') as gmt_modified,\\n    user_name,\\n    login_ip,\\n    method_name,\\n    request_url,\\n    request_params,\\n    response_params,\\n    order_id\\n   \\n        from aiit_operate_log\\n        where is_delete=0 and  order_id=?"}]}, {"spanId":12,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907838,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907844,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"update aiit_operate_log\\n         SET gmt_create=?,\\n            \\n            \\n                gmt_modified=?,\\n            \\n            \\n                user_name=?,\\n            \\n            \\n                login_ip=?,\\n            \\n            \\n                method_name=?,\\n            \\n            \\n                request_url=?,\\n            \\n            \\n                request_params=?,\\n            \\n            \\n                response_params=?,\\n            \\n            \\n                order_id=? \\n        where id=?"}]}]`,operationName:"GET:/firewall/getVisitSourceIpCount",id:8,requestStartTime:"2022-05-17 10:35:07"}]},{header:{userName:"admin",url:"http://localhost:7070/firewall/getHighDangerousPortCount",requestStartTime:"2022-05-17 10:35:07"},body:[{reorganizingSpans:`[{"url":"http://localhost:7070/firewall/getHighDangerousPortCount"}, {"spanId":2,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907766,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907772,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"insert into aiit_operate_log\\n         ( gmt_create,\\n            \\n            \\n            \\n            \\n                login_ip,\\n            \\n            \\n                method_name,\\n            \\n            \\n                request_url,\\n            \\n            \\n            \\n            \\n                order_id ) \\n         values ( ?,\\n            \\n            \\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n                ?,\\n            \\n            \\n            \\n            \\n                ? )"}]}, {"spanId":4,"component":"Lettuce","serviceCode":"demo-application-springboot","peer":"10.0.107.19:6379","endpointName":"Lettuce/GET","startTime":1652754907773,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907780,"parentSpanId":0,"tags":[{"key":"db.type","value":"Redis"},{"key":"db.statement","value":"GET"}]}, {"spanId":6,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907781,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907822,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n     \\n    id\\n    , r_key, r_value, source_ip_group, firewall_exchange_match_id, gmt_create, gmt_update\\n   \\n    from high_risk_port_statistics\\n    order by r_value desc limit ?"}]}, {"spanId":9,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907826,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907828,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"select\\n         \\n    id,\\n    date_format(gmt_create, '%Y-%m-%d %H:%i:%s') as gmt_create,\\n    date_format(gmt_modified, '%Y-%m-%d %H:%i:%s') as gmt_modified,\\n    user_name,\\n    login_ip,\\n    method_name,\\n    request_url,\\n    request_params,\\n    response_params,\\n    order_id\\n   \\n        from aiit_operate_log\\n        where is_delete=0 and  order_id=?"}]}, {"spanId":12,"component":"mysql-connector-java","serviceCode":"demo-application-springboot","peer":"10.0.107.46:3306","endpointName":"Mysql/JDBI/PreparedStatement/execute","startTime":1652754907829,"serviceInstanceName":"41e26a5b546745d5b9a356fb16c5240f@192.168.1.100","endTime":1652754907834,"parentSpanId":0,"tags":[{"key":"db.type","value":"sql"},{"key":"db.instance","value":"zhejiang_mobile"},{"key":"db.statement","value":"update aiit_operate_log\\n         SET gmt_create=?,\\n            \\n            \\n                gmt_modified=?,\\n            \\n            \\n                user_name=?,\\n            \\n            \\n                login_ip=?,\\n            \\n            \\n                method_name=?,\\n            \\n            \\n                request_url=?,\\n            \\n            \\n                request_params=?,\\n            \\n            \\n                response_params=?,\\n            \\n            \\n                order_id=? \\n        where id=?"}]}]`,operationName:"GET:/firewall/getHighDangerousPortCount",id:7,requestStartTime:"2022-05-17 10:35:07"}]}],Y=function(){var s=(0,b.useState)([]),t=(0,I.Z)(s,2),a=t[0],l=t[1];return(0,b.useEffect)(function(){E().then(function(o){var m=o.data,c=JSON.parse(m);l(function(){return c}),console.log(R)})},[]),(0,e.jsx)("div",{className:"link",children:a.length>0?a.map(function(o,m){return(0,e.jsx)(f.ZP,{collapsibleIconRender:function(){return(0,e.jsx)("div",{children:(0,e.jsxs)(y.Z,{size:"small",children:[(0,e.jsx)(y.Z.Item,{label:"\u64CD\u4F5C\u7528\u6237",style:{margin:"0",padding:"0",lineHeight:"1"},children:(0,e.jsx)("p",{className:"primary-blue",children:o.header.userName})}),(0,e.jsx)(y.Z.Item,{label:"\u64CD\u4F5C\u65F6\u95F4",style:{margin:"0",padding:"0",lineHeight:"1"},children:(0,e.jsx)("p",{className:"primary-blue",children:o.header.requestStartTime})}),(0,e.jsx)(y.Z.Item,{label:"url",style:{margin:"0",padding:"0",lineHeight:"1"},children:(0,e.jsx)("p",{className:"primary-blue",children:o.header.url})})]})})},className:"link__item-card",style:{},headerBordered:!0,collapsible:!0,defaultCollapsed:!0,children:(0,e.jsx)("div",{children:(0,e.jsx)(H,{reorganizingSpans:[],body:o.body})})},m+1)}):(0,e.jsx)(j.Z,{style:{textAlign:"center",fontSize:"18px",color:"#C0C4CC",marginTop:"100px"},imageStyle:{width:"400px",height:"400px",margin:"0 auto"},image:F()})})},A=Y},6251:function(q,h,n){q.exports=n.p+"static/empty.a9f51423.png"}}]);
