package com.cosmetic.gg.common.constants;

public class MessageCode {

	public static final int SUCCESS = 1;
	public static final String SUCCESS_MESSAGE = "Thành công";

	public static final int FAILED = 0;
	public static final String FAILED_MESSAGE = "Thất bại";
	
	public static final int NOT_APPROVED_CTS = 3;
	public static final String NOT_APPROVED_CTS_MESSAGE = "Chứng thư số chưa được duyệt";

	public static final int HTTP_SUCCESS = 200;

	public static final Integer AUTHENTICATION_FAILED = 401;
	public static final String AUTHENTICATION_FAILED_MESSAGE = "Xác thực thất bại";

	public static final Integer AUTHENTICATION_WRONG_PASSWORD = 1001;
	public static final String AUTHENTICATION_WRONG_PASSWORD_MESSAGE = "Mật khẩu không trùng khớp";

	public static final int COUNTRY_NOT_EXISTS = 1001;
	public static final String COUNTRY_NOT_EXISTS_MESSAGE = "Quốc gia không tồn tại";
	public static final int PROVINCE_NOT_EXISTS = 1002;
	public static final String PROVINCE_NOT_EXISTS_MESSAGE = "Thành phố không tồn tại";
	public static final int USER_NOT_EXISTS = 1003;
	public static final String USER_NOT_EXISTS_MESSAGE = "Người dùng không tồn tại";
	public static final int ALGORITHM_NOT_EXISTS = 1004;
	public static final String ALGORITHM_NOT_EXISTS_MESSAGE = "Thuật toán không tồn tại";
	public static final int ORGANIZATION_NOT_EXISTS = 1005;
	public static final String ORGANIZATION_NOT_EXISTS_MESSAGE = "Tổ chức không không tồn tại";
	public static final int AGENCY_NOT_EXISTS = 1006;
	public static final String AGENCY_NOT_EXISTS_MESSAGE = "Đại lý không không tồn tại";
	public static final int IDENTITY_NUMBER_NOT_EXISTS = 1007;
	public static final String IDENTITY_NUMBER_NOT_EXISTS_MESSAGE = "Số CMNN/CCCD không tồn tại";
	public static final int EXTENSIONS_NOT_EXISTS = 1008;
	public static final String EXTENSIONS_NOT_EXISTS_MESSAGE = "Tiện ích mở rộng không tồn tại";
	public static final int CA_NOT_EXISTS = 1009;
	public static final String CA_NOT_EXISTS_MESSAGE = "CA không tồn tại";
	public static final int CER_PROFILE_NOT_EXISTS = 1010;
	public static final String CER_PROFILE_NOT_EXISTS_MESSAGE = "Profile không tồn tại";
	public static final int SERVICE_PACKAGE_NOT_EXISTS = 1011;
	public static final String SERVICE_PACKAGE_NOT_EXISTS_MESSAGE = "Gói dịch vụ không tồn tại";
	public static final int DOC_CONTRACT_NOT_EXISTS = 1012;
	public static final String DOC_CONTRACT_NOT_EXISTS_MESSAGE = "Hợp đồng không tồn tại";
	public static final int REVENUE_AND_EXPENDITURE_NOT_EXISTS = 1013;
	public static final String REVENUE_AND_EXPENDITURE_NOT_EXISTS_MESSAGE = "Doanh thu/Chi phí không tồn tại";
	public static final int FORM_NOT_EXISTS = 1014;
	public static final String FORM_NOT_EXISTS_MESSAGE = "Biểu mẫu không tồn tại";
	public static final int FORM_INFOMATINON_NOT_EXISTS = 1015;
	public static final String FORM_INFOMATINON_NOT_EXISTS_MESSAGE = "Thông tin biểu mẫu không tồn tại";
	public static final int ARTICLE_NOT_EXISTS = 1016;
	public static final String ARTICLE_NOT_EXISTS_MESSAGE = "Hồ sơ không tồn tại";
	public static final int SECURITY_SETTINGS_NOT_EXISTS = 1017;
	public static final String SECURITY_SETTINGS_NOT_EXISTS_MESSAGE = "Cài đặt bảo mật không tồn tại";
	public static final int CERT_CERTIFICATE_NOT_EXISTS = 1018;
	public static final String CERT_CERTIFICATE_NOT_EXISTS_MESSAGE = "CTS không tồn tại";
	public static final int CUSTOMER_NOT_EXISTS = 1019;
	public static final String CUSTOMER_NOT_EXISTS_MESSAGE = "Người dùng không tồn tại";
	public static final int REASON_NOT_EXISTS = 1020;
	public static final String REASON_NOT_EXISTS_MESSAGE = "Lý do không tồn tại";
	public static final int ROLES_NOT_EXISTS = 1021;
	public static final String ROLES_NOT_EXISTS_MESSAGE = "Vai trò không tồn tại";
	public static final int ACCOUNT_LOCKED = 1025;
	public static final String ACCOUNT_LOCKED_MESAGE = "Tài khoản đã bị khóa";

	public static final int ACCOUNT_ALREADY_EXISTS = 2001;
	public static final String ACCOUNT_ALREADY_EXISTS_MESSAGE = "Tài khoản đã tồn tại";
	public static final int CODE_ALREADY_EXISTS = 2002;
	public static final String CODE_ALREADY_EXISTS_MESSAGE = "Mã đã tồn tại";
	public static final int IDENTITY_NUMBER_ALREADY_EXISTS = 2003;
	public static final String IDENTITY_NUMBER_ALREADY_EXISTS_MESSAGE = "Số CMNN/CCCD đã tồn tại";
	public static final int PASSWORD_INCORRECT = 2004;
	public static final String PASSWORD_INCORRECT_MESSAGE = "Mật khẩu không khớp";
	public static final int EMAIL_ALREADY_EXISTS = 2005;
	public static final String EMAIL_ALREADY_EXISTS_MESSAGE = "Email đã tồn tại";
	public static final int PHONE_NUMBER_ALREADY_EXISTS = 2006;
	public static final String PHONE_NUMBER_ALREADY_EXISTS_MESSAGE = "Số điện thoại đã tồn tại";
	public static final int PASSWORD_WRONG_FORMAT = 2007;
	public static final String PASSWORD_WRONG_FORMAT_MESSAGE = "Sai định dạng mật khẩu";
	public static final int CREATE_USER_IDP_FAILE = 2008;
	public static final String CREATE_USER_IDP_FAILE_MESSAGE = "Tạo tài khoản trên IDP thất bại";
	public static final int SEND_MAIL_FAILE = 2009;
	public static final String SEND_MAIL_FAILE_MESSAGE = "Gửi mail kích hoạt tài khoản thất bại";
	public static final int CREATE_CERT_FAILED = 2010;
	public static final String CREATE_CERT_FAILED_MESSAGE = "Tạo chứng thư số thất bại";
	public static final int CREATE_KEY_AND_CSR_FAILED = 2011;
	public static final String CREATE_KEY_AND_CSR_FAILED_MESSAGE = "Tạo key và csr thất bại";
	public static final int SEND_MAIL_FAILE_FOR_NOTIFICATION = 2012;
	public static final String SEND_MAIL_FAILE_FOR_NOTIFICATION_MESSAGE = "Gửi mail thông báo cho người dùng thất bại";


	public static final String LIST_IS_EMPTY = "Danh sách trống";
	public static final int GET_CURRENT_USER_FAILED = 2012;
	public static final String GET_CURRENT_USER_FAILED_MESSAGE = "Lấy thông tin của người dùng thất bại";

	public static final int PASSWORD_CANNOT_BE_DUPLICATED_FAILED = 2013;
	public static final String PASSWORD_CANNOT_BE_DUPLICATED_FAILED_MESSAGE = "Mật khẩu cũ và mật khẩu mới không được trùng nhau";
	public static final int INCORRECT_USER_CODE = 2014;
	public static final String INCORRECT_USER_CODE_FAILED_MESSAGE = "Mã người dùng không chính xác";

	public static final String TOKEN_NULL = "Token null";
	public static final String USER_INFO_NULL = "Không tìm thấy thông tin user";

	public static final int CONNECT_MINIO = 701;
	public static final String CONNECT_MINIO_MESSAGE = "Kết nối tới minIO thất bại";
	public static final int SAVE_MINIO = 702;
	public static final String SAVE_MINIO_MESSAGE = "Lưu file vào minIO thất bại";
	public static final int BUCKET_NOT_FOUND = 703;
	public static final String BUCKET_NOT_FOUND_MESSAGE = "Không tìm thấy bucket name";

	public static final int CA_REQUIRED = 2978;
	public static final String CA_REQUIRED_MESSAGE = "Thiếu CA";
	public static final int PROFILE_REQUIRED = 2979;
	public static final String PROFILE_REQUIRED_MESSAGE = "Thiếu profile";
	public static final int ARTICLE_REQUIRED = 2980;
	public static final String ARTICLE_REQUIRED_MESSAGE = "Thiếu hồ sơ";
	public static final int TIME_CTS_REQUIRED = 2981;
	public static final String TIME_CTS_REQUIRED_MESSAGE = "Thời gian của CTS bắt buộc nhập";
	public static final int OBJECT_NAME_REQUIRED = 2982;
	public static final String OBJECT_NAME_REQUIRED_MESSAGE = "Object name bắt buộc phải có giá trị";
	public static final int BUCKET_NAME_REQUIRED = 2982;
	public static final String BUCKET_NAME_REQUIRED_MESSAGE = "Bucket name bắt buộc phải có giá trị";
	public static final int REQUIRED = 2982;
	public static final String REQUIRED_MESSAGE = "bắt buộc phải nhập giá trị";
	public static final int USERNAME_MAX_SIZE = 2983;
	public static final String USERNAME_MAX_SIZE_MESSAGE = "Tên đăng nhập không được quá 20 ký tự";
	public static final int PHONE_NUMBER_INVALID = 2984;
	public static final String PHONE_NUMBER_INVALID_MESSAGE = "Sai định dạng số điện thoại";
	public static final int EMAIL_INVALID = 2985;
	public static final String EMAIL_INVALID_MESSAGE = "Sai định dạng email";
	public static final int EMAIL_MAX_SIZE = 2986;
	public static final String EMAIL_MAX_SIZE_MESSAGE = "Email chỉ được tối đa 256 ký tự";
	public static final int PASSWORD_INVALID = 2987;
	public static final String PASSWORD_INVALID_MESSAGE = "Mật khẩu tối thiếu là 8 ký tự và tối đa là 20 ký tự bao gồm chữ hoa, chữ thường, số, ký tự đặc biệt";
	public static final int MAX_NUMBER_FILE = 2988;
	public static final String MAX_NUMBER_FILE_MESSAGE = "Vượt quá số lượng file! Số file tối đa là 3";
	public static final int USER_UPDATE_FAILED = 2989;
	public static final String USER_UPDATE_FAILED_MESSAGE = "Cập nhật không thành công do mã người dùng không chính xác";
	public static final int COUNTRY_NOT_MODIFIED = 2990;
	public static final String COUNTRY_NOT_MODIFIED_MESSAGE = "Quốc gia không được sửa đổi";
	public static final int NAME_NOT_MODIFIED = 2991;
	public static final String NAME_NOT_MODIFIED_MESSAGE = "Họ và tên không được sửa đổi";
	public static final int OTP_NOT_EMPTY = 2992;
	public static final String OTP_IMAGE_NOT_EMPTY_MESSAGE = "Mã OTP không được để trống";
	public static final int BIRTH_DAY_NOT_EMPTY = 2993;
	public static final String BIRTH_DAY_IMAGE_NOT_EMPTY_MESSAGE = "Ngày sinh không được để trống";
	public static final int USER_TYPE_NOT_EMPTY = 2994;
	public static final String USER_TYPE_NOT_EMPTY_MESSAGE = "UserType không được để trống";
	public static final int USERNAME_NOT_EMPTY = 2995;
	public static final String USERNAME_NOT_EMPTY_MESSAGE = "Tên đăng nhập không được để trống";
	public static final int PASSWORD_NOT_EMPTY = 2996;
	public static final String PASSWORD_NOT_EMPTY_MESSAGE = "Mật khẩu không được để trống";
	public static final int CONFIRM_PASSWORD_NOT_EMPTY = 2997;
	public static final String CONFIRM_PASSWORD_NOT_EMPTY_MESSAGE = "Nhập lại mật khẩu không được để trống";
	public static final int COMMITMENT_NOT_EMPTY = 2998;
	public static final String COMMITMENT_NOT_EMPTY_MESSAGE = "Yêu cầu chấp nhận điều khoản thoả thuận dịch vụ";
	public static final int AGENCY_NOT_EMPTY = 2999;
	public static final String AGENCY_NOT_EMPTY_MESSAGE = "Đại lý không được để trống";
	public static final int ROLE_NOT_EMPTY = 3000;
	public static final String ROLE_NOT_EMPTY_MESSAGE = "Vai trò không được để trống";

	public static final int NAME_NOT_EMPTY = 3001;
	public static final String NAME_NOT_EMPTY_MESSAGE = "Họ và tên không được để trống";
	public static final int IDENTITY_NUMBER_NOT_EMPTY = 3002;
	public static final String IDENTITY_NUMBER_NOT_EMPTY_MESSAGE = "Số CMND/CCCD/HC không được để trống";
	public static final int SEX_NOT_EMPTY = 3003;
	public static final String SEX_NOT_EMPTY_MESSAGE = "Giới tính không được để trống";
	public static final int ISSUEER_DATE_NOT_EMPTY = 3004;
	public static final String ISSUEER_DATE_NOT_EMPTY_MESSAGE = "Ngày cấp không được để trống";
	public static final int ISSUEER_BY_NOT_EMPTY = 3005;
	public static final String ISSUEER_BY_NOT_EMPTY_MESSAGE = "Nơi cấp không được để trống";
	public static final int PERMANENT_ADDRESS_NOT_EMPTY = 3006;
	public static final String PERMANENT_ADDRESS_NOT_EMPTY_MESSAGE = "Đia chỉ thường trú không được để trống";
	public static final int CURRENT_ADDRESS_NOT_EMPTY = 3007;
	public static final String CURRENT_ADDRESS_NOT_EMPTY_MESSAGE = "Địa chỉ hiện tại không được để trống";
	public static final int COUNTRY_NOT_EMPTY = 3008;
	public static final String COUNTRY_NOT_EMPTY_MESSAGE = "Quốc gia không được để trống";
	public static final int PROVINCE_NOT_EMPTY = 3050;
	public static final String PROVINCE_NOT_EMPTY_MESSAGE = "Tỉnh/Thành phố không được để trống";
	public static final int EMAIL_NOT_EMPTY = 3009;
	public static final String EMAIL_NOT_EMPTY_MESSAGE = "Email không được để để trống";
	public static final int PHONE_NUMBER_NOT_EMPTY = 3010;
	public static final String PHONE_NUMBER_NOT_EMPTY_MESSAGE = "Số điện thoại không được để trống";
	public static final int FRONT_IDENTITY_IMAGE_NOT_EMPTY = 3011;
	public static final String FRONT_IDENTITY_IMAGE_NOT_EMPTY_MESSAGE = "Ảnh CMND/CCCD mặt trước không được để trống";
	public static final int BACK_IDENTITY_IMAGE_NOT_EMPTY = 3012;
	public static final String BACK_IDENTITY_IMAGE_NOT_EMPTY_MESSAGE = "Ảnh CMND/CCCD mặt sau không được để trống";

	public static final int ACCOUNT_VERIFIED = 3013;
	public static final String ACCOUNT_VERIFIED_MESSAGE = "Tài khoản đã kích hoạt";
	public static final int TOKEN_TIME_OUT = 3014;
	public static final String TOKEN_TIME_OUT_MESSAGE = "Hết thời gian xác thực! Yêu cầu truy cập lại email của bạn để lấy lại token";
	public static final int EXCEEDED_THE_ALLOWED_NUMBER_OF_REQUESTS = 3015;
	public static final String EXCEEDED_THE_ALLOWED_NUMBER_OF_REQUESTS_MESSAGE = "Vượt quá số lượng yêu cầu được cho phép";
	public static final int ACCOUNT_NOT_ACTIVE = 3016;
	public static final String ACCOUNT_NOT_ACTIVE_MESSAGE = "Tài khoản chưa được kích hoạt";
	public static final int LOGIN_FAILED = 3017;
	public static final String LOGIN_FAILED_MESSAGE = "Đăng nhập thất bại";
	public static final String INFO_LOGIN_FAILED_MESSAGE = "Thông tin đăng nhập không hợp lệ";
	public static final int UPDATE_USER_IDP_FAILED = 3018;
	public static final String UPDATE_USER_IDP_FAILED_MESSAGE = "Cập nhật người dùng trên IDP thất bại";
	public static final int GENERATE_OTP_FAILED = 3019;
	public static final String GENERATE_OTP_FAILED_MESSAGE = "Tạo OTP thất bại";
	public static final int VALIDATE_OTP_FAILED = 3020;
	public static final String VALIDATE_OTP_FAILED_MESSAGE = "Kiểm tra OTP thất bại";
	public static final int GET_IDENTITY_IMAGE_FAILED = 3021;
	public static final String GET_IDENTITY_IMAGE_FAILED_MESSAGE = "Không thể lấy ảnh CMND/ CCCD/ Hộ chiếu của người dùng";

	public static final int REVOKE_CTS_FAILED = 3022;
	public static final String REVOKE_CTS_FAILED_MESSAGE = "Thu hồi chứng thư số thất bại";
	public static final int SUSPENDED_CTS_FAILED = 3023;
	public static final String SUSPENDED_CTS_FAILED_MESSAGE = "Tạm dừng chứng thư số thất bại";
	public static final int RENEW_CTS_FAILED = 3024;
	public static final String RENEW_CTS_FAILED_MESSAGE = "Gia hạn chứng thư số thất bại";
	public static final int RESUMES_CTS_FAILED = 3025;
	public static final String RESUMES_CTS_FAILED_MESSAGE = "Kích hoạt chứng thư số thất bại";
	public static final int OLD_PASSWORD_IS_NOT_INCORECT_FAILED = 3026;
	public static final String OLD_PASSWORD_IS_NOT_INCORECT_FAILED_MESSAGE = "Mật khẩu cũ không đúng";
	public static final int PASSWORD_IS_NOT_INCORECT_FAILED = 3027;
	public static final String PASSWORD_IS_NOT_INCORECT_FAILED_MESSAGE = "Mật khẩu không đúng";

	public static final int INVALID_USER_CREDENTIALS_FAILED = 3028;
	public static final String INVALID_USER_CREDENTIALS_FAILED_MESSAGE = "Thông tin đăng nhập không hợp lệ";
	public static final int TOKEN_INVALID = 3029;
	public static final String TOKEN_INVALID_MESSAGE = "Token không hợp lệ";
	public static final int CREATE_SEAT_FAILED = 3030;
	public static final String CREATE_SEAT_FAILED_MESSAGE = "Tạo seat thất bại";
	public static final int STEP_NOT_EMPTY = 3031;
	public static final String STEP_NOT_EMPTY_MESSAGE = "Step không được để trống";

	public static final int TIME_ID_NOT_EXISTS = 3032;
	public static final String TIME_ID_NOT_EXISTS_MESSAGE = "ID của thời gian không tồn tại";
	public static final int TIME_DURATION_NOT_EXISTS = 3033;
	public static final String TIME_DURATION_NOT_EXISTS_MESSAGE = "Khoảng thời gian không tồn tại";
	public static final int TIME_UNIT_NOT_EXISTS = 3034;
	public static final String TIME_UNIT_NOT_EXISTS_MESSAGE = "Đơn vị thời gian không tồn tại";
	public static final int MISSING_ARTICLE = 3035;
	public static final String MISSING_ARTICLE_MESSAGE = "Thiếu hồ sơ";
	public static final int MISSING_PROFILE = 3036;
	public static final String MISSING_PROFILE_MESSAGE = "Thiếu trường trong profile";
	public static final int EMAIL_PHONE_IS_CHANGED = 3037;
	public static final String EMAIL_PHONE_IS_CHANGED_MESSAGE = "Thông tin email hoặc số điện thoại đã bị thay đổi! Vui lòng kiểm tra email để cập nhật";
	public static final int INCORRECT_FILE_FORMAT = 3038;
	public static final String INCORRECT_FILE_FORMAT_MESSAGE = "Không đúng định dạng file";
	public static final int COUNTRY_MAX_LENGTH = 3039;
	public static final String COUNTRY_MAX_LENGTH_MESSAGE = "Quốc gia chỉ được phép có 2 ký tự";

	public static final int DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_RENEWAL = 3041;
	public static final String DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_RENEWAL_MESSAGE = "Chứng thư số không đủ điều kiện để thực hiện gia hạn";
	public static final int DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_APPROVALL = 3042;
	public static final String DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_APPROVALL_MESSAGE = "Chứng thư số không đủ điều kiện để thực hiện duyệt";
	public static final int DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_RECOVERY = 3043;
	public static final String DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_RECOVERY_MESSAGE = "Chứng thư số không đủ điều kiện để thực hiện thu hồi";
	public static final int DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_SUSPENDED = 3044;
	public static final String DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_SUSPENDED_MESSAGE = "Chứng thư số không đủ điều kiện để thực hiện tạm dừng";
	public static final int DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_ACTIVATION = 3045;
	public static final String DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_FOR_ACTIVATION_MESSAGE = "Chứng thư số không đủ điều kiện để thực hiện kích hoạt do trạng thái không phải là tạm dừng";
	public static final int CONNECT_EJBCA_FAILED = 3046;
	public static final String CONNECT_EJBCA_FAILED_MESSAGE = "Kết nối đến EJBCA thất bại";
	public static final int CONNECT_NEKS_FAILED = 3047;
	public static final String CONNECT_NEKS_FAILED_MESSAGE = "Tạo cặp khoá và CSR thất bại";
	public static final int REASON_MANDATORY = 3048;
	public static final String REASON_MANDATORY_MESSAGE = "Yêu cầu chọn lý do";
	public static final int CTS_RENEWED = 3049;
	public static final String CTS_RENEWED_MESSAGE = "Chứng thư số đã được yêu cầu gia hạn 1 lần. Không thể tiếp tục gia hạn";
	public static final int FILE_NO_FILE_TO_VERIFY = 3050;
	public static final String FILE_NO_FILE_TO_VERIFY_MESSAGE = "Không có tệp để xác minh";
	public static final int FILE_DUPLICATE_UPLOAD = 3051;
	public static final String FILE_DUPLICATE_UPLOAD_MESSAGE = "Tải tệp tin bị trùng";
	public static final int CTS_CONFIRMED = 3052;
	public static final String CTS_CONFIRMED_MESSAGE = "Chứng thư số đã được người dùng xác nhận ";
	public static final int CONVERT_BASE64_FAILED = 3053;
	public static final String CONVERT_BASE64_FAILED_MESSAGE = "Chuyển đổi chứng thư số sang base64 thất bại";
	public static final int CRL_DOWNLOAD_FAILED = 3054;
	public static final String CRL_DOWNLOAD_FAILED_MESSAGE = "Tải về CRL thất bại";

	public static final int CHECK_VALID_TIME_FAILED = 3055;
	public static final String CHECK_VALID_TIME_FAILED_MESSAGE = "Kiểm tra hiệu lực chứng thư số thất bại";
	public static final int CHECK_REVOKE_FAILED = 3056;
	public static final String CHECK_REVOKE_FAILED_MESSAGE = "Kiểm tra trạng thái thu hồi chứng thư số thất bại";

	public static final int GET_LIST_ID_AGENCY_FAILED = 3057;
	public static final String GET_LIST_ID_AGENCY_FAILED_MESSAGE = "Lấy danh sách đại lý thất bại";

	public static final String SEND_MAIL_OTP_SUCCESS_MESSAGE = "Gửi OTP về email thành công";

	public static final int FAILD_TO_CREATE_EFORM = 3054;
	public static final String FAILD_TO_CREATE_EFORM_MESSAGE = "Tạo e-form thất bại";
	public static final int FAILED_TO_SIGN_EFORM = 3055;
	public static final String FAILED_TO_SIGN_EFORM_MESSAGE = "Ký e-form thất bại";

	public static final int WRONG_USER_ID = 3058;
	public static final String WRONG_USER_ID_MESSAGE = "Mã người dùng không chính xác";
	public static final int WRONG_DEVICE = 3059;
	public static final String WRONG_DEVICE_MESSAGE = "Mã thiết bị không chính xác";
	public static final int WRONG_IDENTITY_NO = 3060;
	public static final String WRONG_IDENTITY_NO_MESSAGE = "CMND/ CCCD/ Hộ chiếu không chính xác";
	public static final int WRONG_EMAIL = 3061;
	public static final String WRONG_EMAIL_MESSAGE = "Email không chính xác";
	public static final int WRONG_PHONE = 3062;
	public static final String WRONG_PHONE_MESSAGE = "Số điện thoại không chính xác";
	public static final int WRONG_ORGANIZATION = 3063;
	public static final String WRONG_ORGANIZATION_MESSAGE = "Tổ chức không chính xác";
	public static final int WRONG_ORGANIZATION_UNIT = 3064;
	public static final String WRONG_ORGANIZATION_UNIT_MESSAGE = "Đơn vị tổ chức không chính xác";
	public static final int DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_TO_DOWNLOAD_FILE = 3065;
	public static final String DIGITAL_CERTIFICATE_IS_NOT_ELIGIBLE_TO_DOWNLOAD_FILE_MESSAGE = "Chứng thư số phải được kích hoạt mới đủ điều kiện để tải tệp tin";
	public static final int DO_NOT_DELETE_YOUR_OWN_ACCOUNT = 3066;
	public static final String DO_NOT_DELETE_YOUR_OWN_ACCOUNT_MESSAGE = "Không được xóa tài khoản của chính mình";
	public static final int PASSWORD_OVERDATE = 3067;
	public static final String PASSWORD_OVERDATE_MESSAGE = "Mật khẩu đã quá hạn vui lòng đổi mật khẩu mới. Yêu cầu mật khẩu không được giống 5 lần gần nhất";
	public static final int INFO_PASSWORD_MANAGER = 3068;
	public static final String INFO_PASSWORD_MANAGER_MESSAGE = "Không tìm thấy thông tin thời hạn đổi mật khẩu của tài khoản";
	public static final int ADMIN_TTCNTT_UPDATE = 3069;
	public static final String ADMIN_TTCNTT_UPDATE_MESSAGE = "Chỉ Admin TTCNTT được chỉnh sửa";
	public static final int INFORMATION_OF_USER_FAILED = 3078;
	public static final String INFORMATION_OF_USER_FAILED_MESSAGE = "Sai thông tin người dùng";
	
	public static final int SEND_MAIL_FAILED = 3079;
	public static final String SEND_MAIL_FAILED_MESSAGE = "Gửi mail thất bại";
	public static final int VALID_EMPTY = 3079;
	public static final String VALID_EMPTY_MESSAGE = "Tất cả giá trị không được để trống";
	public static final String THE_NUMBER_OF_OTP_CHECKS_IS_OVER_MESSAGE = "Tài khoản của bạn đã bị khóa do nhập quá số lần kiểm tra OTP";
	public static final int OTP_EXPIRE = 3180;
	public static final String OTP_EXPIRE_MESSAGE = "Mã OTP hết hạn";
	public static final int WRONG_OTP_CODE = 3181;
	public static final String WRONG_OTP_CODE_MESSAGE = "Sai mã OTP";
	public static final int THE_NUMBER_OF_TESTS_ENDED = 3182;
	public static final String THE_NUMBER_OF_TESTS_ENDED_MESSAGE = "Hết số lượt kiểm tra OTP, tài khoản của bạn sẽ bị khóa";
	
	public static final int NOT_AUTHORIZED_TO_PERFORM_THIS_ACTION = 3183;
	public static final String NOT_AUTHORIZED_TO_PERFORM_THIS_ACTION_MESSAGE = "Không đủ quyền để thực hiện thao tác này";
	
	public static final int FORM_INFORMATION_CODE_NOT_EXISTS = 3184;
	public static final String FORM_INFORMATION_CODE_NOT_EXISTS_MESSAGE = "Thông tin biểu mẫu không tồn tại";
	public static final int FORM_CODE_NOT_EXISTS = 3184;
	public static final String FORM_CODE_NOT_EXISTS_MESSAGE = "Biểu mẫu không tồn tại";
	public static final int FORM_CODE_EXISTS = 3185;
	public static final String FORM_CODE_EXISTS_MESSAGE = "Mã biểu mẫu đã tồn tại";
	public static final int SERVICE_PACKAGE_EXISTS = 3185;
	public static final String SERVICE_PACKAGE_EXISTS_MESSAGE = "Mã gói dịch vụ đã tồn tại";
	public static final int TIME_NOT_EXISTS = 3185;
	public static final String TIME_NOT_EXISTS_MESSAGE = "Thời gian không tồn tại";
	
	
	//BIDV
	public static final int ALIAS_NOT_EMPTY = 4079;
	public static final String ALIAS_NOT_EMPTY_MESSAGE = "Alias không được rỗng!";
	public static final int CSR_NOT_EMPTY = 4080;
	public static final String CSR_NOT_EMPTY_MESSAGE = "CSR không được rỗng!";
	public static final int SUBEJCTDN_NOT_EMPTY = 4081;
	public static final String SUBEJCTDN_NOT_EMPTY_MESSAGE = "Subject DN không được rỗng!";
	public static final int FILE_NOT_EMPTY = 4082;
	public static final String FILE_NOT_EMPTY_MESSAGE = "Tài liệu không được rỗng!";
	public static final int FILE_WRONG = 4083;
	public static final String FILE_WRONG_MESSAGE = "Sai định dạng file!";


	// validate userInfor mb bank
	public static final int VALIDATE_FAILED = 9999;
	public static final String VALID_DOB_FAIL = "Sai định dạng dob";
	public static final String VALID_ISSUEDATE_FAIL = "Sai định dạng issueDate";
	public static final String VALID_DOCUMENT_FAIL = "Số lượng document không hợp lệ";
	public static final String VALID_OU_FAIL = "Organization Unit không hợp lệ";
	public static final String VALID_UID_FAIL = "Mã định danh không hợp lệ";

	// validate certificate request agribank
	public static final String VALIDATE_COMMON_NAME_FAILED = "Không được để trống họ và tên";
	public static final String VALIDATE_ACCOUNT_IPCAS_FAILED = "Không được để trống mã tài khoản IPCAS";
	public static final String VALIDATE_BRANCH_FAILED = "Không được để trống mã chi nhánh";
	public static final String VALIDATE_EMPLOYEE_FAILED = "Không được để trống mã nhân sự";
	public static final String VALIDATE_EMAIL_FAILED = "Không được để trống địa chỉ email";
	public static final String VALIDATE_TAX_CODE_FAILED = "Không được để trống mã số thuế";
	public static final String VALIDATE_IDENTITY_CODE_FAILED = "Không được để trống CMND/CCCD/Hộ chiếu";
	public static final String VALIDATE_TIME_PROFILE_FAILED = "Không được để trống thời gian";
	public static final String VALIDATE_REGIONS_FAILED = "Không được để trống vùng miền";

	public static final int VALIDATE_ALGORITHM_CODE_FAILED = 5050;
	public static final String VALIDATE_ALGORITHM_CODE_FAILED_MESSAGE = "Không được để trống mã thuật toán";
	public static final int VALIDATE_ALGORITHM_NAME_FAILED = 5051;
	public static final String VALIDATE_ALGORITHM_NAME_FAILED_MESSAGE = "Không được để trống tên thuật toán";
	
	//Validate certificate CA request
	public static final int VALIDATE_CER_IDENTITY_FAILED = 5000;
	public static final String VALIDATE_CER_IDENTITY_FAILED_MESSAGE = "Không được để trống mã định danh nhà cung cấp CA";
	public static final int VALIDATE_CER_CODE_FAILED = 5001;
	public static final String VALIDATE_CER_CODE_FAILED_MESSAGE = "Không được để trống mã nhà cung cấp CA";
	public static final int VALIDATE_CER_NAME_FAILED = 5002;
	public static final String VALIDATE_CER_NAME_FAILED_MESSAGE = "Không được để trống tên nhà cung cấp CA";
	public static final int VALIDATE_COUNTRY_ID_FAILED = 5003;
	public static final String VALIDATE_COUNTRY_ID_FAILED_MESSAGE = "Không được để trống mã định danh quốc gia của nhà cung cấp CA";
	public static final int VALIDATE_COUNTRY_CODE_FAILED = 5004;
	public static final String VALIDATE_COUNTRY_CODE_FAILED_MESSAGE = "Không được để trống mã quốc gia của nhà cung cấp CA";
	public static final int VALIDATE_COUNTRY_NAME_FAILED = 5005;
	public static final String VALIDATE_COUNTRY_NAME_FAILED_MESSAGE = "Không được để trống tên quốc gia của nhà cung cấp CA";
	public static final int CER_CODE_ALREADY_EXISTS = 5006;
	public static final String CER_CODE_ALREADY_EXISTS_MESSAGE = "Mã nhà cung cấp đã tồn tại";
	public static final int CER_IDENTITY_IN_LIST_ALREADY_EXISTS = 5007;
	public static final String CER_IDENTITY_IN_LIST_ALREADY_EXISTS_MESSAGE = "Tồn tại mã định danh trong danh sách không hợp lệ";
	
}
