package com.cosmetic.gg.entity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import com.cosmetic.gg.common.enums.ERole;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Entity
@Table(name= "role")
public class Role extends EntityBase{

    @Enumerated(EnumType.STRING)
    @Column(name = "name")
    private ERole name;
}
