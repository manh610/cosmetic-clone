package com.cosmetic.gg.common.enums;

public enum ErrorCode {

	SUCCESS("SYS-0000", "Thành công", "Success"),
	FAILURE("SYS-0001", "Thất bại", "Failure"),
	NOT_FOUND("SYS-0002", "Dữ liệu không tồn tại", "Data is not exist"),
	EXCEPTION("SYS-9999", "Lỗi ngoại lệ", "Exception"),
	UNKNOWN("UNKNOWN", "Lỗi không xác định", "Unknown Error"),
	
	
	INVALID_DATA("SYS-0003", "Dữ liệu không hợp lệ", "Data is invalid"),
	INVALID_IMAGE("SYS-0004", "Không hỗ trợ định dạng file này", "This file format is not supported"),
	INVALID_STATUS("SYS_006", "Trạng thái không hợp lệ", "Status is invalid"),
	EXIST_DATA("SYS-0007", "Dữ liệu đã tồn tại", "Data is exist"),
	EXIST_NAME("SYS-0008", "Tên đã tồn tại", "Name is exist"),
	EXIST_CODE("SYS-0009", "Ký hiệu đã tồn tại", "Code is exist"),
	EXIST_ID("SYS-0010", "Mã đã tồn tại", "Code is exist"),
	ERROR_CONVERT_IMAGE("SYS-0011", "Lỗi chuyển đổi file", "Can not convert file"),
	
	
	
	INVALID_PARENT("SYS-0008", "Mã cấp cha không hợp lệ", "Id of parent is invalid"),
	CHILD_REFERENCE("SYS-0009", "Dữ liệu đang có dữ liệu cấp con tham chiếu", "Children are reference"),
	
	USER_EXIST_USERNAME("USER-0002", "Tài khoản người dùng đã tồn tại", "Username is exist"),
	USER_EXIST_EMAIL("USER-0003", "Email đã tồn tại", "Email is exist"),
	USER_EXIST_PHONE("USER-0004", "Số điện thoại đã tồn tại", "Phone is exist"),
	USER_EXIST_CITIZEN_NUMBER("USER-0005", "Số căn cước công dân đã tồn tại", "Citizen number is exist"),
	USER_INVALID_RANK("USER-0007", "Xếp hạng của người dùng không hợp lệ", "Rank of user is invalid"),
	USER_INVALID_TYPE("USER-0009", "Loại người dùng không hợp lệ", "Type of user is invalid"),
	
	ADDRESS_INVALID_DEFAULT("ADDRESS-001", "Trạng thái mặc định không hợp lệ", "Default status is invalid"),
	
	PRODUCT_INVALID_DATE("PRO-001", "Ngày không hợp lệ", "Date is invalid"),
	PRODUCT_INVALID_SKIN_TYPE("PRO-002", "Loại da không hợp lệ", "Skin type is invalid"),
	PRODUCT_INVALID_VALUE_DETAIL("PRO-003", "Chi tiết sản phẩm không hợp lệ", "Value detail is invalid"),
	
	ORDER_INAVLID("ORD-001", "Đơn đặt hàng không hợp lệ", "Order product is invalid"),
	
	DISCOUNT_INVALID_DELETE("DISCOUNT-001", "Dữ liệu đang có đối tượng tham chiếu", "Object are reference"),
	DISCOUNT_INVALID("DISCOUNT-002", "Mã giảm giá không hợp lệ", "Discount is invalid"),
	
	INVALID_PRODUCT("SYS-0011", "Thông tin sản phẩm không hợp lệ", "Product is invalid"),
	INVALID_USER("SYS-0012", "Thông tin người dùng không hợp lệ", "User is invalid"),
	
	
	INVALID_CREDENTIALS("AUTH-001", "Thông tin đăng nhập không hợp lệ", "Login is inavalid"),
	USER_DISABLED("AUTH-002", "", "");
	
	
	private String code;
	private final String vn;
	private final String en;
	
	ErrorCode(String code, String vn, String en) {
		this.code = code;
		this.vn = vn;
		this.en = en;
	}
	
	public String getCode() { return code; }
	
	public void setCode(String code) { this.code = code; }
	
	public String getVn() { return vn; }
	
	public String getEn() { return en; }
}
