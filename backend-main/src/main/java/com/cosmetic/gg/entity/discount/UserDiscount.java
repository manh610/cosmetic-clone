package com.cosmetic.gg.entity.discount;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import com.cosmetic.gg.entity.EntityBase;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Table(name = "user_discount", indexes = {
    @Index(name = "idx_user_discount_discount_id", columnList = "discount_id"),
    @Index(name = "idx_user_discount_user_id", columnList = "user_id")
})
@Entity
@Getter
@Setter
public class UserDiscount extends EntityBase{
	
	@Column(name = "is_use")
	private boolean isUse = false;
	
	@NotEmpty(message = "Id of user not empty")
	@NotNull(message = "Id of user can not null")
	@Column(name = "user_id")
	private String userId;
	
	@NotEmpty(message = "Id of discount can not empty")
	@NotNull(message = "Id of discount can not null")
	@Column(name = "discount_id")
	private String discountId;
}
