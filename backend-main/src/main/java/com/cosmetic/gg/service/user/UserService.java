package com.cosmetic.gg.service.user;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.common.enums.EUserRank;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.entity.User;
import com.cosmetic.gg.entity.discount.Discount;
import com.cosmetic.gg.model.UserModel;

public interface UserService {
	
	Map<String, Object> search(
			String keyword,
			EStatus status,
			String roleId,
			EUserRank userRank,
			Integer pageIndex,
			Integer pageSize);
	
	UserModel getByKey(String key);
	
	List<Error> validator(UserModel userModel);
	
	UserModel create(UserModel userModel) throws IOException;
	
	UserModel update(UserModel userModel);
	
	ErrorCode changeImage(MultipartFile imageFile, String id);
	
	Error delete(String id);
	
	UserModel detail(String id);
	
	Error blockAccount(String id);
	
	Error recoverAccount(String id);
	
	UserModel register(UserModel userModel);
	
	Error changePassword(String oldPassword, String newPassword, String username);
	
	User getUserFromToken(String accessToken);
	
	LocalDateTime getExpireTimeToken(String accessToken);
	
	List<Discount> getDiscountByUser(String id, Boolean use);
}
